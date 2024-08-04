module Amber.Coverage.Check (
        CoverageError(..),
        coverageCheckModule,
    ) where

import Control.Lens
import Control.Monad
import Polysemy
import Polysemy.Reader
import Polysemy.Error
import Data.Maybe
import Data.Foldable

import Amber.Syntax.Abstract
import Amber.Typing.Context
import Amber.Typing.Check
import qualified Amber.Coverage.PatternAlgebra as CA

data CoverageError
    = MissingPatternsError Text [Pat]
    deriving (Show)

type E a = forall r. Members [Reader GlobalEnv, Error CoverageError] r => Sem r a

coverageCheckModule :: Module -> E ()
coverageCheckModule = module'

module' :: Module -> E ()
module' = traverse_ directive

directive :: Directive -> E ()
directive (RecDef ident eqs) = do
    env <- ask
    let sig = fromJust $ preview (recDefs . at ident . _Just . recSignature) env
    missing <- filterM (typeable ident sig) $ foldr1 CA.intersection [CA.complement env p | Equation p _ <- eqs]
    case missing of
        [] -> pure ()
        _ -> throw $ MissingPatternsError ident missing
directive _ = pure ()

typeable :: Members '[Reader GlobalEnv] r => Text -> Exp -> Pat -> Sem r Bool
typeable ident sig p =
    runError (checkPat ident sig p) <&> \case
        Left (_ :: TypeError) -> False
        Right _ -> True
