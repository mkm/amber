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
patFree SubjectP = mempty
patFree (AppP p1 p2) = patFree p1 <> subPatFree p2

subPatFree :: SubPat -> Set Name
subPatFree (VarP x) = S.singleton x
subPatFree (ConP _ ps) = mconcat $ map subPatFree ps
subPatFree (ForcedP _) = mempty

headFree :: Head -> Set Name
headFree (Local x) = S.singleton x
headFree (Con _) = S.empty
headFree (GlobalRec _) = mempty
headFree (GlobalInd _) = mempty

expFree :: Exp -> Set Name
expFree (AppE h es) = headFree h <> mconcat (map expFree es)
expFree UnivE = S.empty
expFree (FunE x e1 e2) = expFree e1 <> (S.delete x $ expFree e2)
expFree (AmbE e1 e2) = expFree e1 <> expFree e2
