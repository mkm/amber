module Amber.Syntax.Equivalence (
        AlphaConv,
        Equiv(..),
    ) where

import Control.Monad
import Polysemy.Reader hiding (Local)

import Amber.Util.Misc
import Amber.Util.Polysemy
import Amber.Util.PartIso (PartIso)
import qualified Amber.Util.PartIso as PartIso
import Amber.Syntax.Abstract

type AlphaConv = PartIso Name Name

class Equiv a where
    equiv :: a -> a -> Eff '[Reader AlphaConv] Bool

instance Equiv Name where
    equiv x y = asks $ PartIso.member x y

instance Equiv Head where
    equiv (Local x) (Local y) = equiv x y
    equiv (GlobalRec x) (GlobalRec y) = pure $ x == y
    equiv (GlobalInd x) (GlobalInd y) = pure $ x == y
    equiv _ _ = pure False

instance Equiv Exp where
    equiv (AppE h es) (AppE h' es') =
        equiv h h' <&&> fmap and (zipWithM equiv es es')
    equiv UnivE UnivE = pure True
    equiv (FunE x e1 e2) (FunE x' e1' e2') =
        equiv e1 e1' <&&> local (PartIso.insert x x') (equiv e2 e2')
    equiv _ _ = pure False
