{-# LANGUAGE TemplateHaskell, RecursiveDo #-}
module Amber.Syntax.Resolver (
        resolveModule,
    ) where

import Prelude hiding (exp)
import Control.Lens hiding (Context)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Fix
import Data.Foldable
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M

import Amber.Util.Reader
import Amber.Syntax.Name
import qualified Amber.Syntax.Concrete as C
import qualified Amber.Syntax.Abstract as A

type Res = ReaderT Context
type PatRes m = StateT (Map Text Name) (Res m)

data Context =
    Context {
        _recDecs :: Map Text Status,
        _indDecs :: Map Text Status,
        _conTypes :: Map Text Text,
        _currentDef :: Maybe CurrentDef,
        _bindings :: Map Text Name
    }

data Status = Declared | Defined
    deriving (Show, Eq, Ord, Enum)

data CurrentDef = CurrentRec Text [A.Equation] | CurrentInd Text [A.Constructor]

makeLenses ''Context

resolveModule :: MonadFix m => C.Module -> m A.Module
resolveModule m = runReaderT (module' m) emptyContext

emptyContext :: Context
emptyContext =
    Context {
        _recDecs = M.empty, 
        _indDecs = M.empty,
        _conTypes = M.empty,
        _currentDef = Nothing,
        _bindings = M.empty
    }

module' :: MonadFix m => C.Module -> Res m A.Module
module' [] = emittingCurrentDef $ pure []
module' (C.RecDec ident ty : mod) =
    emittingCurrentDef $ do
        ty' <- exp ty
        mod' <- local (recDecs . at ident ?~ Declared) $ module' mod
        pure $ A.RecDec ident ty' : mod'
module' (C.IndDec ident ty : mod) =
    emittingCurrentDef $ do
        ty' <- exp ty
        mod' <- local (indDecs . at ident ?~ Declared) $ module' mod
        pure $ A.IndDec ident ty' : mod'
module' (C.Equation p e : mod) = mdo
    ((subject, p'), vars) <- local (bindings .~ vars) $ runStateT (pat p) M.empty
    e' <- local (bindings .~ vars) $ exp e
    view currentDef >>= \case
        Just (CurrentRec ident eqs) | ident == subject ->
            local (currentDef ?~ CurrentRec subject (eqs ++ [A.Equation p' e'])) $ module' mod
        _ -> emittingCurrentDef . local (currentDef ?~ CurrentRec subject [A.Equation p' e']) $ module' mod
module' (C.Constructor ident ty : mod) = do
    ty' <- exp ty
    view currentDef >>= \case
        Just (CurrentInd fam cons) | fam == target ty' ->
            local (currentDef ?~ CurrentInd fam (cons ++ [A.Constructor ident ty'])) $ module' mod
        _ -> emittingCurrentDef . local (currentDef ?~ CurrentInd (target ty') [A.Constructor ident ty']) $ module' mod

emittingCurrentDef :: MonadFix m => Res m A.Module -> Res m A.Module
emittingCurrentDef mod =
    view currentDef >>= \case
        Nothing ->
            local (currentDef .~ Nothing) mod
        Just (CurrentRec ident eqs) ->
            locals [currentDef .~ Nothing, recDecs . at ident ?~ Defined] do
                (:) (A.RecDef ident eqs) <$> mod
        Just (CurrentInd fam cons) ->
            locals [currentDef .~ Nothing, indDecs . at fam ?~ Defined] do
                locals [conTypes . at ident ?~ target ty | A.Constructor ident ty <- cons] do
                    (:) (A.IndDef fam cons) <$> mod

pat :: MonadFix m => C.Pat -> PatRes m (Text, A.Pat)
pat (C.VarP subject) = pure (subject, A.SubjectP)
pat C.WildP = error "pattern subject is _"
pat (C.AppP p1 p2) = do
    (subject, p1') <- pat p1
    p2' <- subPat p2
    pure (subject, A.AppP p1' p2')

subPat :: MonadFix m => C.Pat -> PatRes m A.SubPat
subPat (C.VarP ident) =
    view (conTypes . at ident) >>= \case
        Just _ -> pure $ A.ConP ident []
        Nothing -> do
            x <- patName ident
            at ident ?= x
            pure $ A.VarP x
subPat C.WildP = do
    x <- patName "_"
    at "_" ?= x
    pure $ A.VarP x
subPat (C.ForcedP e) = do
    vars <- get
    e' <- lift $ exp e
    pure $ A.ForcedP e'
subPat (C.AppP p1 p2) = do
    p1' <- subPat p1
    p2' <- subPat p2
    pure $ A.appSP1 p1' p2'

exp :: MonadFix m => C.Exp -> Res m A.Exp
exp (C.VarE ident) =
    asum <$> traverse ($ ident) [localVar, conVar, globalRec, globalInd] >>= \case
        Nothing -> error $ "`" ++ T.unpack ident ++ "` not found"
        Just e -> pure e
        {-
    view (bindings . at ident) >>= \case
        Nothing ->
            view (recDecs . at ident) >>= \case
                Nothing -> view (indDecs . at ident) >>= \case
                    Nothing -> error $ "`" ++ T.unpack ident ++ "` not found"
                    Just _ -> pure $ A.AppE (A.GlobalInd ident) []
                Just _ -> pure $ A.AppE (A.GlobalRec ident) []
        Just name -> pure $ A.AppE (A.Local name) []
        -}
exp (C.AppE e1 e2) = do
    e1' <- exp e1
    e2' <- exp e2
    pure $ A.app e1' [e2']
exp C.UnivE = pure A.UnivE
exp (C.FunE ident e1 e2) = do
    let ident' = fromMaybe "_" ident
    e1' <- exp e1
    name <- distinctName (canonicalName ident') <$> view bindings
    {-
    (scope, name) <- case ident of
        Nothing -> (,) id <$> newName "_"
        Just ident -> do
            name <- newName ident
            pure (bindings . at ident ?~ name, name)
    -}
    e2' <- local (bindings . at ident' ?~ name) $ exp e2
    pure $ A.FunE name e1' e2'

localVar :: Monad m => Text -> Res m (Maybe A.Exp)
localVar ident = view (bindings . at ident) <&> fmap \x -> A.AppE (A.Local x) []

conVar :: Monad m => Text -> Res m (Maybe A.Exp)
conVar ident = view (conTypes . at ident) <&> fmap \_ -> A.AppE (A.Con ident) []

globalRec :: Monad m => Text -> Res m (Maybe A.Exp)
globalRec ident = view (recDecs . at ident) <&> fmap \_ -> A.AppE (A.GlobalRec ident) []

globalInd :: Monad m => Text -> Res m (Maybe A.Exp)
globalInd ident = view (indDecs . at ident) <&> fmap \_ -> A.AppE (A.GlobalInd ident) []

target :: A.Exp -> Text
target (A.FunE _ _ ty) = target ty
target (A.AppE (A.GlobalInd ident) _) = ident

patName :: Monad m => Text -> PatRes m Name
patName x = distinctName (canonicalName x) <$> gets M.elems
