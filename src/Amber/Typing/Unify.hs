module Amber.Typing.Unify (
        Unify(..),
    ) where

import Polysemy.Reader hiding (Local)
import Polysemy.Error

import Amber.Util.Polysemy
import Amber.Util.PartIso (PartIso)
import qualified Amber.Util.PartIso as PartIso
import Amber.Syntax.Abstract

type AlphaConv = PartIso Name Name

{-
data UnifyError a = UnifyError a a
    deriving (Show)
-}
type UnifyError = ()

class Unify a where
    unify :: a -> a -> Eff '[Reader AlphaConv, Error UnifyError] ()

    default unify :: Eq a => a -> a -> Eff '[Reader AlphaConv, Error UnifyError] ()
    unify x y
        | x == y = pure ()
        | otherwise = different x y

instance Unify Text where

instance Unify a => Unify [a] where
    unify [] [] = pure ()
    unify (x : xs) (y : ys) = unify x y >> unify xs ys
    unify xs ys = different xs ys

instance Unify Name where
    unify x y = asks (PartIso.member x y) >>= \case
        True -> pure ()
        False -> different x y

instance Unify Head where
    unify (Local x) (Local y) = unify x y
    unify (Con x) (Con y) = unify x y
    unify (GlobalRec x) (GlobalRec y) = unify x y
    unify (GlobalInd x) (GlobalInd y) = unify x y
    unify x y = different x y

instance Unify Exp where
    unify (AppE h es) (AppE h' es') = do
        unify h h'
        unify es es'
    unify UnivE UnivE = pure ()
    unify (FunE x e1 e2) (FunE x' e1' e2') = do
        unify e1 e1'
        local (PartIso.insert x x') $ unify e2 e2'
    unify e e' = different e e'

different :: a -> a -> Eff '[Error UnifyError] ()
different _ _ = throw ()
