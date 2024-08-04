{-# LANGUAGE TemplateHaskell, RecursiveDo #-}
module Amber.Syntax.Resolver (
        resolveModule,
    ) where

import Prelude hiding (exp)
import Control.Lens hiding (Context)
import Polysemy
import Polysemy.Reader
import Polysemy.State
import Data.Foldable
import Data.Maybe
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M

import Amber.Util.Polysemy
import Amber.Syntax.Name
import qualified Amber.Syntax.Concrete as C
import qualified Amber.Syntax.Abstract as A

type Names = Map Text Name
type Res a = Eff '[Reader Context] a
type PatRes a = forall r. Members '[Reader Context] r => Sem (State Names ': r) (Sem r a)

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

resolveModule :: C.Module -> Eff '[] A.Module
resolveModule = runReader emptyContext . module'

emptyContext :: Context
emptyContext =
    Context {
        _recDecs = M.empty, 
        _indDecs = M.empty,
        _conTypes = M.empty,
        _currentDef = Nothing,
        _bindings = M.empty
    }

module' :: C.Module -> Res A.Module
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
module' (C.Equation p e : mod) = do
    (vars, cont) <- runState M.empty (pat p)
    (subject, p') <- local (bindings .~ vars) cont
    e' <- local (bindings .~ vars) $ exp e
    asks (view currentDef) >>= \case
        Just (CurrentRec ident eqs) | ident == subject ->
            local (currentDef ?~ CurrentRec subject (eqs ++ [A.Equation p' e'])) $ module' mod
        _ -> emittingCurrentDef . local (currentDef ?~ CurrentRec subject [A.Equation p' e']) $ module' mod
module' (C.Constructor ident ty : mod) = do
    ty' <- exp ty
    asks (view currentDef) >>= \case
        Just (CurrentInd fam cons) | fam == target ty' ->
            local (currentDef ?~ CurrentInd fam (cons ++ [A.Constructor ident ty'])) $ module' mod
        _ -> emittingCurrentDef . local (currentDef ?~ CurrentInd (target ty') [A.Constructor ident ty']) $ module' mod

emittingCurrentDef :: Member (Reader Context) r => Sem r A.Module -> Sem r A.Module
emittingCurrentDef mod =
    asks (view currentDef) >>= \case
        Nothing ->
            local (currentDef .~ Nothing) mod
        Just (CurrentRec ident eqs) ->
            locals [currentDef .~ Nothing, recDecs . at ident ?~ Defined] do
                (:) (A.RecDef ident eqs) <$> mod
        Just (CurrentInd fam cons) ->
            locals [currentDef .~ Nothing, indDecs . at fam ?~ Defined] do
                locals [conTypes . at ident ?~ target ty | A.Constructor ident ty <- cons] do
                    (:) (A.IndDef fam cons) <$> mod

pat :: C.Pat -> PatRes (Text, A.Pat)
pat (C.VarP subject) = pure $ pure (subject, A.SubjectP)
pat C.WildP = error "pattern subject is _"
pat (C.ForcedP _) = error "pattern subject is a forced expression"
pat (C.AppP p1 p2) = do
    p1' <- pat p1
    p2' <- subPat p2
    pure do
        (subject, p1'') <- p1'
        p2'' <- p2'
        pure (subject, A.AppP p1'' p2'')

subPat :: C.Pat -> PatRes A.SubPat
subPat (C.VarP ident) =
    (asks . view $ conTypes . at ident) >>= \case
        Just _ -> pure . pure $ A.ConP ident []
        Nothing -> do
            x <- patName ident
            modify $ M.insert ident x
            pure . pure $ A.VarP x
subPat C.WildP = do
    x <- patName "_"
    modify @Names $ M.insert "_" x
    pure . pure $ A.VarP x
subPat (C.ForcedP e) = pure do
    e' <- exp e
    pure $ A.ForcedP e'
subPat (C.AppP p1 p2) = do
    p1' <- subPat p1
    p2' <- subPat p2
    pure $ A.appSP1 <$> p1' <*> p2'

exp :: C.Exp -> Res A.Exp
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
    name <- distinctName (canonicalName ident') <$> asks (view bindings)
    {-
    (scope, name) <- case ident of
        Nothing -> (,) id <$> newName "_"
        Just ident -> do
            name <- newName ident
            pure (bindings . at ident ?~ name, name)
    -}
    e2' <- local (bindings . at ident' ?~ name) $ exp e2
    pure $ A.FunE name e1' e2'

localVar :: Text -> Res (Maybe A.Exp)
localVar ident = (asks . view $ bindings . at ident) <&> fmap \x -> A.AppE (A.Local x) []

conVar :: Text -> Res (Maybe A.Exp)
conVar ident = (asks . view $ conTypes . at ident) <&> fmap \_ -> A.AppE (A.Con ident) []

globalRec :: Text -> Res (Maybe A.Exp)
globalRec ident = (asks . view $ recDecs . at ident) <&> fmap \_ -> A.AppE (A.GlobalRec ident) []

globalInd :: Text -> Res (Maybe A.Exp)
globalInd ident = (asks . view $ indDecs . at ident) <&> fmap \_ -> A.AppE (A.GlobalInd ident) []

target :: A.Exp -> Text
target (A.FunE _ _ ty) = target ty
target (A.AppE (A.GlobalInd ident) _) = ident
target _ = undefined

patName :: Text -> Eff '[State Names] Name
patName x = distinctName (canonicalName x) <$> gets @(Map Text Name) M.elems
