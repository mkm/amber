module Amber.Syntax.Free (
        patFree,
        subPatFree,
        headFree,
        expFree,
    ) where

import Data.Set (Set)
import qualified Data.Set as S

import Amber.Syntax.Abstract

patFree :: Pat -> Set Name
patFree SubjectP = S.empty
patFree (AppP p1 p2) = patFree p1 `S.union` subPatFree p2

subPatFree :: SubPat -> Set Name
subPatFree (VarP x) = S.singleton x
subPatFree (ConP _ ps) = S.unions $ map subPatFree ps
subPatFree (ForcedP _) = S.empty

headFree :: Head -> Set Name
headFree (Local x) = S.singleton x
headFree (Con _) = S.empty
headFree (GlobalRec _) = S.empty
headFree (GlobalInd _) = S.empty

expFree :: Exp -> Set Name
expFree (AppE h es) = headFree h `S.union` S.unions (map expFree es)
expFree UnivE = S.empty
expFree (FunE x e1 e2) = expFree e1 `S.union` (S.delete x $ expFree e2)
