module Amber.Syntax.Subst (
        Subst(..),
        subst1,
    ) where

import Data.Maybe
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import Amber.Syntax.Abstract
import Amber.Syntax.Free

class Subst t t' | t -> t' where
    subst :: Map Name Exp -> t -> t'

subst1 :: Subst t t' => Name -> Exp -> t -> t'
subst1 x e = subst $ M.singleton x e

instance Subst Name Exp where
    subst s x = fromMaybe (AppE (Local x) []) (M.lookup x s)

instance Subst Head Exp where
    subst s (Local x) = subst s x
    subst _ h@(Con _) = AppE h []
    subst _ h@(GlobalRec _) = AppE h []
    subst _ h@(GlobalInd _) = AppE h []

instance Subst Exp Exp where
    subst s (AppE h es) = app (subst s h) (subst s <$> es)
    subst _ e@UnivE = e
    subst s (FunE x e1 e2) = FunE x' (subst s e1) (subst s' e2)
        where
            freeVars = S.unions [S.insert y (expFree e') | (y, e') <- M.toList s]
            x' = distinctName x freeVars
            s' = M.insert x (AppE (Local x') []) s
