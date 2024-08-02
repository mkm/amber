module Amber.Typing.Check (
        TypeError(..),
        checkModule,
        checkPat,
    ) where

import Prelude hiding (exp, head)
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except
import Polysemy
import qualified Polysemy.Reader as PR
import qualified Polysemy.Error as PE
import Data.Foldable
import Data.Monoid
import Data.Text (Text)
import qualified Data.Map as M

import qualified Amber.Util.PartIso as PartIso
import Amber.Syntax.Abstract
import Amber.Syntax.Subst
import Amber.Typing.Context
import Amber.Typing.Unify
import Amber.Typing.Normalise

type Check m = StateT GlobalEnv (ExceptT TypeError m)
type CheckPat m = StateT LocalEnv (WriterT (Ap (CheckExp m) ()) (Check m))
type CheckExp m = ReaderT LocalEnv (Check m)

data TypeError
    = UnboundLocalVarError Name
    | UnboundGlobalVarError Text
    | AlreadyDefinedError Text
    | NotFunError Exp
    | NotUnivError Exp
    | ExpUnifyError Exp Exp
    | InEquation Text Equation TypeError
    | InRHS Exp Exp Exp TypeError
    | InPat Text Pat TypeError
    | InExp Exp TypeError
    | InExpAt Exp Exp TypeError
    | InExpApp Exp [Exp] TypeError
    | InHead Head TypeError
    deriving (Show)

checkModule :: Monad m => Module -> ExceptT TypeError m GlobalEnv
checkModule mod = module' mod `evalStateT` mempty

checkPat :: Members '[PR.Reader GlobalEnv, PE.Error TypeError] r => Text -> Exp -> Pat -> Sem r ()
checkPat ident sig p = do
    env <- PR.ask
    case runIdentity . runExceptT $ topPat ident sig p `evalStateT` env of
        Left err -> PE.throw err
        Right _ -> pure ()

module' :: Monad m => Module -> Check m GlobalEnv
module' mod = do
    mapM_ directive mod
    get

directive :: Monad m => Directive -> Check m ()
directive (RecDec ident ty) = (`runReaderT` mempty) $ do
    univ ty
    recDefs . at ident ?= RecDefDec { _recSignature = ty, _recStatus = RecDeclared }
directive (IndDec ident ty) = (`runReaderT` mempty) $ do
    univFamily ty
    indDefs . at ident ?= IndDefDec { _indSignature = ty, _indStatus = IndDeclared }
directive (RecDef ident eqs) = use (recDefs . at ident) >>= \case
    Nothing -> throwError $ UnboundGlobalVarError ident
    Just dd@(view recStatus -> RecDeclared) -> do
        genv <- get
        sig <- expNF $ dd ^. recSignature
        traverse_ (equation ident sig) eqs
        recDefs . at ident ?= (dd & recStatus .~ RecDefined eqs)
    _ -> throwError $ AlreadyDefinedError ident
directive (IndDef fam cons) = use (indDefs . at fam) >>= \case
    Nothing -> throwError $ UnboundGlobalVarError fam
    Just dd@(view indStatus -> IndDeclared) -> do
        genv <- get
        sig <- expNF $ dd ^. indSignature
        traverse_ (constructor fam) cons
        indDefs . at fam ?= (dd & indStatus .~ IndDefined cons)

equation :: Monad m => Text -> Exp -> Equation -> Check m ()
equation ident sig eq@(Equation p e) = withError (InEquation ident eq) $ do
    (env, ty) <- topPat ident sig p
    expAt ty e `runReaderT` env
{-
    ((ty, env), forced) <- runWriterT $ pat ident sig p `runStateT` mempty
    getAp forced `runReaderT` env
    expAt ty e `runReaderT` env
    -}

constructor :: Monad m => Text -> Constructor -> Check m ()
constructor fam (Constructor ident ty) = do
    univ ty `runReaderT` mempty
    conTypes . at ident ?= fam

topPat :: Monad m => Text -> Exp -> Pat -> Check m (LocalEnv, Exp)
topPat ident sig p = do
    ((ty, env), forced) <- runWriterT $ pat ident sig p `runStateT` mempty
    getAp forced `runReaderT` env
    pure (env, ty)

pat :: Monad m => Text -> Exp -> Pat -> CheckPat m Exp
pat ident sig p = withError (InPat ident p) $ pat' ident sig p

pat' :: Monad m => Text -> Exp -> Pat -> CheckPat m Exp
pat' ident sig SubjectP = pure sig
pat' ident sig (AppP p1 p2) =
    pat ident sig p1 >>= \case
        FunE x ty' ty -> do
            subPat ty' p2
            lift $ expNF (subst1 x (patExp p2) ty)
        ty -> throwError $ NotFunError ty

subPat :: Monad m => Exp -> SubPat -> CheckPat m ()
subPat sig (VarP x) = bindings . at x ?= sig
subPat sig (ConP ident ps) =
    (lift . use $ conTypes . at ident) >>= \case
        Just fam -> do
            cons <- lift . use $ indDefs . at fam . _Just . indStatus . _IndDefined
            case [ty | Constructor ident' ty <- cons, ident' == ident] of
                [] -> throwError $ UnboundGlobalVarError ident
                [ty] -> do
                    ty' <- lift $ expNF ty
                    conPat sig ty' ps
subPat sig (ForcedP e) = tell $ Ap (expAt sig e)

conPat :: Monad m => Exp -> Exp -> [SubPat] -> CheckPat m ()
conPat sig ty [] = do
    lenv <- get
    lift . lift $ unifyExp sig ty `runReaderT` lenv
conPat sig (FunE x ty' ty) (p : ps) = do
    subPat ty' p
    tyNF <- lift $ expNF (subst1 x (patExp p) ty)
    conPat sig tyNF ps

patExp :: SubPat -> Exp
patExp (VarP x) = AppE (Local x) []
patExp (ConP ident ps) = AppE (Con ident) (map patExp ps)
patExp (ForcedP e) = e

exp :: Monad m => Exp -> CheckExp m Exp
exp e = withError (InExp e) $ exp' e

expAt :: Monad m => Exp -> Exp -> CheckExp m ()
expAt ty e = withError (InExpAt ty e) do
    ty' <- exp' e
    unifyExp ty' ty

exp' :: Monad m => Exp -> CheckExp m Exp
exp' (AppE h es) = do
    ty0 <- head h
    traverse_ exp es
    appTy ty0 es >>= expNF
exp' UnivE = pure UnivE
exp' (FunE x e1 e2) = do
    univ e1
    local (bindings . at x ?~ e1) $ univ e2
    pure UnivE

head :: Monad m => Head -> CheckExp m Exp
head h = withError (InHead h) $ head' h

head' :: Monad m => Head -> CheckExp m Exp
head' (Local x) = view (bindings . at x) >>= \case
    Nothing -> throwError $ UnboundLocalVarError x
    Just ty -> expNF ty
head' (Con ident) = use (conTypes . at ident) >>= \case
    Nothing -> throwError $ UnboundGlobalVarError ident
    Just fam -> do
        cons <- use (indDefs . at fam . traverse . indStatus . _IndDefined)
        case [ty | Constructor ident' ty <- cons, ident' == ident] of
            [] -> throwError $ UnboundGlobalVarError ident
            [ty] -> expNF ty
head' (GlobalRec ident) = preuse (recDefs . at ident . traverse . recSignature) >>= \case
    Nothing -> throwError $ UnboundGlobalVarError ident
    Just ty -> expNF ty
head' (GlobalInd ident) = preuse (indDefs . at ident . traverse . indSignature) >>= \case
    Nothing -> throwError $ UnboundGlobalVarError ident
    Just ty -> expNF ty

univ :: Monad m => Exp -> CheckExp m ()
univ = expAt UnivE

univFamily :: Monad m => Exp -> CheckExp m ()
univFamily UnivE = pure ()
univFamily (FunE _ _ ty) = univFamily ty
univFamily ty = throwError $ NotUnivError ty

appTy :: Monad m => Exp -> [Exp] -> CheckExp m Exp
appTy ty es = withError (InExpApp ty es) $ appTy' ty es

appTy' :: Monad m => Exp -> [Exp] -> CheckExp m Exp
appTy' ty [] = pure ty
appTy' (FunE x ty1 ty2) (e : es) = do
    expAt ty1 e
    ty2' <- lift $ expNF (subst (M.singleton x e) ty2)
    appTy ty2' es
appTy' ty _ = throwError $ NotFunError ty

unifyExp :: Monad m => Exp -> Exp -> CheckExp m ()
unifyExp e1 e2 = view bindings >>= \bs ->
    unifyWith (const $ ExpUnifyError e1 e2) (PartIso.diagonal $ M.keysSet bs) e1 e2

expNF :: MonadState GlobalEnv m => Exp -> m Exp
expNF e = get >>= flip expNormalForm e
