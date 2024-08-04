module Amber.Typing.Check (
        TypeError(..),
        checkModule,
        checkPat,
    ) where

import Prelude hiding (exp, head)
import Control.Lens
import Control.Monad
import Polysemy
import Polysemy.Reader hiding (Local)
import Polysemy.Writer
import Polysemy.State
import Polysemy.Error
import Data.Foldable
import qualified Data.Map as M

import Amber.Util.Panic
import Amber.Util.Polysemy
import qualified Amber.Util.PartIso as PartIso
import Amber.Syntax.Abstract
import Amber.Syntax.Subst
import Amber.Typing.Context
import Amber.Typing.Unify
import Amber.Typing.Normalise

type Check a = Eff '[State GlobalEnv, Error TypeError] a
type CheckPat a = forall r. Members '[State GlobalEnv, Error TypeError, State LocalEnv, Writer CheckExpBox] r => Sem r a
type CheckExp a = Eff '[State GlobalEnv, Error TypeError, Reader LocalEnv] a

newtype CheckExpBox = CheckExpBox { getCheckExpBox :: forall r. Members '[State GlobalEnv, Error TypeError, Reader LocalEnv] r => Sem r () }

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

checkModule :: Module -> Eff '[Error TypeError] GlobalEnv
checkModule = evalState @GlobalEnv mempty . module'

checkPat :: Text -> Exp -> Pat -> Eff '[Reader GlobalEnv, Error TypeError] ()
checkPat ident sig p = do
    env <- ask
    void . evalState @GlobalEnv env $ topPat ident sig p

module' :: Module -> Check GlobalEnv
module' mod = do
    mapM_ directive mod
    get

directive :: Directive -> Check ()
directive (RecDec ident ty) = runReader @LocalEnv mempty do
    univ ty
    modify $ recDefs . at ident ?~ RecDefDec { _recSignature = ty, _recStatus = RecDeclared }
directive (IndDec ident ty) = runReader @LocalEnv mempty do
    univFamily ty
    modify $ indDefs . at ident ?~ IndDefDec { _indSignature = ty, _indStatus = IndDeclared }
directive (RecDef ident eqs) = (gets . view $ recDefs . at ident) >>= \case
    Nothing -> throw $ UnboundGlobalVarError ident
    Just dd@(view recStatus -> RecDeclared) -> do
        sig <- expNF $ dd ^. recSignature
        traverse_ (equation ident sig) eqs
        modify $ recDefs . at ident ?~ (dd & recStatus .~ RecDefined eqs)
    _ -> throw $ AlreadyDefinedError ident
directive (IndDef fam cons) = (gets . view $ indDefs . at fam) >>= \case
    Nothing -> throw $ UnboundGlobalVarError fam
    Just dd@(view indStatus -> IndDeclared) -> do
        traverse_ (constructor fam) cons
        modify $ indDefs . at fam ?~ (dd & indStatus .~ IndDefined cons)
    Just _ -> throw $ AlreadyDefinedError fam

equation :: Text -> Exp -> Equation -> Check ()
equation ident sig eq@(Equation p e) = withError (InEquation ident eq) $ do
    (env, ty) <- topPat ident sig p
    runReader env $ expAt ty e
{-
    ((ty, env), forced) <- runWriter $ pat ident sig p `runState` mempty
    getAp forced `runReader` env
    expAt ty e `runReader` env
    -}

constructor :: Text -> Constructor -> Check ()
constructor fam (Constructor ident ty) = do
    runReader @LocalEnv mempty $ univ ty
    modify $ conTypes . at ident ?~ fam

topPat :: Text -> Exp -> Pat -> Eff '[State GlobalEnv, Error TypeError] (LocalEnv, Exp)
topPat ident sig p = do
    (forced, (env, ty)) <- runWriter . runState mempty $ pat ident sig p
    runReader env $ getCheckExpBox forced
    pure (env, ty)

pat :: Text -> Exp -> Pat -> CheckPat Exp
pat ident sig p = withError (InPat ident p) $ pat' ident sig p

pat' :: Text -> Exp -> Pat -> CheckPat Exp
pat' _ sig SubjectP = pure sig
pat' ident sig (AppP p1 p2) =
    pat ident sig p1 >>= \case
        FunE x ty' ty -> do
            subPat ty' p2
            expNF (subst1 x (patExp p2) ty)
        ty -> throw $ NotFunError ty

subPat :: Exp -> SubPat -> CheckPat ()
subPat sig (VarP x) = modify $ bindings . at x ?~ sig
subPat sig (ConP ident ps) =
    (gets . view $ conTypes . at ident) >>= \case
        Nothing -> impossible
        Just fam -> do
            cons <- gets . view $ indDefs . at fam . _Just . indStatus . _IndDefined
            case [ty | Constructor ident' ty <- cons, ident' == ident] of
                [] -> throw $ UnboundGlobalVarError ident
                [ty] -> do
                    ty' <- expNF ty
                    conPat sig ty' ps
                _ -> impossible
subPat sig (ForcedP e) = tell $ CheckExpBox (expAt sig e)

conPat :: Exp -> Exp -> [SubPat] -> CheckPat ()
conPat sig ty [] = do
    lenv <- get @LocalEnv
    runReader lenv $ unifyExp sig ty
conPat sig (FunE x ty' ty) (p : ps) = do
    subPat ty' p
    tyNF <- expNF (subst1 x (patExp p) ty)
    conPat sig tyNF ps
conPat _ _ _ = impossible

patExp :: SubPat -> Exp
patExp (VarP x) = AppE (Local x) []
patExp (ConP ident ps) = AppE (Con ident) (map patExp ps)
patExp (ForcedP e) = e

exp :: Exp -> CheckExp Exp
exp e = withError (InExp e) $ exp' e

expAt :: Exp -> Exp -> CheckExp ()
expAt ty e = withError (InExpAt ty e) do
    ty' <- exp' e
    unifyExp ty' ty

exp' :: Exp -> CheckExp Exp
exp' (AppE h es) = do
    ty0 <- head h
    traverse_ exp es
    appTy ty0 es >>= expNF
exp' UnivE = pure UnivE
exp' (FunE x e1 e2) = do
    univ e1
    local (bindings . at x ?~ e1) $ univ e2
    pure UnivE

head :: Head -> CheckExp Exp
head h = withError (InHead h) $ head' h

head' :: Head -> CheckExp Exp
head' (Local x) = (asks . view $ bindings . at x) >>= \case
    Nothing -> throw $ UnboundLocalVarError x
    Just ty -> expNF ty
head' (Con ident) = (gets . view $ conTypes . at ident) >>= \case
    Nothing -> throw $ UnboundGlobalVarError ident
    Just fam -> do
        cons <- gets . view $ indDefs . at fam . traverse . indStatus . _IndDefined
        case [ty | Constructor ident' ty <- cons, ident' == ident] of
            [] -> throw $ UnboundGlobalVarError ident
            [ty] -> expNF ty
            _ -> impossible
head' (GlobalRec ident) = (gets . preview $ recDefs . at ident . traverse . recSignature) >>= \case
    Nothing -> throw $ UnboundGlobalVarError ident
    Just ty -> expNF ty
head' (GlobalInd ident) = (gets . preview $ indDefs . at ident . traverse . indSignature) >>= \case
    Nothing -> throw $ UnboundGlobalVarError ident
    Just ty -> expNF ty

univ :: Exp -> CheckExp ()
univ = expAt UnivE

univFamily :: Exp -> CheckExp ()
univFamily UnivE = pure ()
univFamily (FunE _ _ ty) = univFamily ty
univFamily ty = throw $ NotUnivError ty

appTy :: Exp -> [Exp] -> CheckExp Exp
appTy ty es = withError (InExpApp ty es) $ appTy' ty es

appTy' :: Exp -> [Exp] -> CheckExp Exp
appTy' ty [] = pure ty
appTy' (FunE x ty1 ty2) (e : es) = do
    expAt ty1 e
    ty2' <- expNF (subst (M.singleton x e) ty2)
    appTy ty2' es
appTy' ty _ = throw $ NotFunError ty

expNF :: Exp -> Eff '[State GlobalEnv] Exp
expNF e = do
    env <- get @GlobalEnv
    runReader env $ expNormalForm e

unifyExp :: Exp -> Exp -> CheckExp ()
unifyExp e1 e2 = do
    bs <- asks $ view bindings
    mapError (\() -> ExpUnifyError e1 e2) . runReader (PartIso.diagonal $ M.keysSet bs) $ unify e1 e2
    -- unifyWith (const $ ExpUnifyError e1 e2) (PartIso.diagonal $ M.keysSet bs) e1 e2

instance Semigroup CheckExpBox where
    CheckExpBox a <> CheckExpBox b = CheckExpBox (a >> b)

instance Monoid CheckExpBox where
    mempty = CheckExpBox (pure ())
