module Amber.Typing.Normalise (
        expNormalForm,
    ) where

import Prelude hiding (exp)
import Control.Lens
import Control.Applicative
import Control.Monad
import Polysemy.Reader
import Polysemy.Writer
import Polysemy.Fail
import Polysemy.NonDet
import Data.Foldable1
import Data.List.NonEmpty (nonEmpty)

import Amber.Util.Polysemy
import Amber.Shell.Pretty
import Amber.Shell.Panic
import Amber.Syntax.Abstract
import Amber.Syntax.Subst
import Amber.Syntax.Pretty
import Amber.Typing.Context

expNormalForm :: Exp -> Eff '[Reader GlobalEnv] Exp
expNormalForm = expNF

expNF :: Exp -> Eff '[Reader GlobalEnv] Exp
expNF (AppE h@(GlobalRec x) es) = do
    es' <- traverse expNF es
    eqs <- asks . view $ recDefs . at x . _Just . recStatus . _RecDefined
    matches <- runNonDet . asum $ map (failToNonDet . matchEquation es') eqs
    case nonEmpty matches of
        Nothing -> pure $ AppE h es'
        Just es -> traverse expNF es >>= foldrM1 ambExp
expNF (AppE h es) = AppE h <$> traverse expNF es
expNF e@UnivE = pure e
expNF (FunE x ty1 ty2) = FunE x <$> expNF ty1 <*> expNF ty2
expNF (AmbE e1 e2) = do
    e1' <- expNF e1
    e2' <- expNF e2
    ambExp e1' e2'

matchEquation :: [Exp] -> Equation -> Eff [Reader GlobalEnv, Fail] Exp
matchEquation es (Equation p e) = do
    (env, es') <- runWriter $ matchPat es p
    pure $ app (subst (env ^. bindings) e) es'

matchPat :: [Exp] -> Pat -> Eff [Reader GlobalEnv, Fail, Writer LocalEnv] [Exp]
matchPat es SubjectP = pure es
matchPat es (AppP p1 p2) = do
    (e0 : es') <- matchPat es p1
    matchSubPat e0 p2
    pure es'

matchSubPat :: Exp -> SubPat -> Eff [Reader GlobalEnv, Fail, Writer LocalEnv] ()
matchSubPat e (VarP x) = tell $ mempty & bindings . at x ?~ e
matchSubPat (AppE (Con ident') es) (ConP ident ps)
    | ident' == ident && length es == length ps = void $ zipWithM matchSubPat es ps
matchSubPat _ (ForcedP _) = pure ()
matchSubPat _ _ = fail ""

ambExp :: Exp -> Exp -> Eff '[] Exp
ambExp (AppE (Con ident1) es1) (AppE (Con ident2) es2)
    | ident1 == ident2 = AppE (Con ident1) <$> zipWithM ambExp es1 es2
ambExp e1 e2 = pure $ AmbE e1 e2
