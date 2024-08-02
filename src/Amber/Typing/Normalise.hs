module Amber.Typing.Normalise (
        expNormalForm,
    ) where

import Prelude hiding (exp)
import Control.Lens
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except

import Amber.Syntax.Abstract
import Amber.Syntax.Subst
import Amber.Typing.Context

type Norm = ReaderT GlobalEnv
type Match m = WriterT LocalEnv (ExceptT () (Norm m))

expNormalForm :: Monad m => GlobalEnv -> Exp -> m Exp
expNormalForm env e = expNF e `runReaderT` env

expNF :: Monad m => Exp -> Norm m Exp
expNF (AppE h@(GlobalRec x) es) = do
    es' <- traverse expNF es
    eqs <- view $ recDefs . at x . _Just . recStatus . _RecDefined
    matches <- runExceptT . asum $ map (matchEquation es') eqs
    case matches of
        Left () -> pure $ AppE h es'
        Right e' -> expNF e'
expNF (AppE h es) = AppE h <$> traverse expNF es
expNF e@UnivE = pure e
expNF (FunE x ty1 ty2) = FunE x <$> expNF ty1 <*> expNF ty2

matchEquation :: Monad m => [Exp] -> Equation -> ExceptT () (Norm m) Exp
matchEquation es (Equation p e) = do
    (es', env) <- runWriterT $ matchPat es p
    pure $ app (subst (env ^. bindings) e) es'

matchPat :: Monad m => [Exp] -> Pat -> Match m [Exp]
matchPat es SubjectP = pure es
matchPat es (AppP p1 p2) =
    matchPat es p1 >>= \case
        [] -> throwError ()
        (e0 : es') -> matchSubPat e0 p2 >> pure es'

matchSubPat :: Monad m => Exp -> SubPat -> Match m ()
matchSubPat e (VarP x) = tell $ mempty & bindings . at x ?~ e
matchSubPat (AppE (Con ident') es) (ConP ident ps)
    | ident' == ident && length es == length ps = void $ zipWithM matchSubPat es ps
matchSubPat _ (ForcedP _) = pure ()
matchSubPat _ _ = throwError ()
