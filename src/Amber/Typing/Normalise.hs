module Amber.Typing.Normalise (
        expNormalForm,
    ) where

import Prelude hiding (exp)
import Control.Lens
import Control.Applicative
import Control.Monad
import Polysemy
import Polysemy.Reader
import Polysemy.Writer
import Polysemy.Fail
import Polysemy.NonDet

import Amber.Util.Polysemy
import Amber.Syntax.Abstract
import Amber.Syntax.Subst
import Amber.Typing.Context

-- type Norm = ReaderT GlobalEnv
-- type Match m = WriterT LocalEnv (ExceptT () (Norm m))

expNormalForm :: Exp -> Eff '[Reader GlobalEnv] Exp
expNormalForm = expNF

expNF :: Exp -> Eff '[Reader GlobalEnv] Exp
expNF (AppE h@(GlobalRec x) es) = do
    es' <- traverse expNF es
    eqs <- asks . view $ recDefs . at x . _Just . recStatus . _RecDefined
    matches <- runNonDet . asum $ map (failToNonDet . matchEquation es') eqs
    case matches of
        [] -> pure $ AppE h es'
        [e'] -> expNF e'
expNF (AppE h es) = AppE h <$> traverse expNF es
expNF e@UnivE = pure e
expNF (FunE x ty1 ty2) = FunE x <$> expNF ty1 <*> expNF ty2

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
