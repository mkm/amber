module Amber.Typing.Unify (
        Unify(..),
    ) where

import Control.Applicative
import Polysemy.Reader hiding (Local)
import Polysemy.NonDet

import Amber.Util.Polysemy
import Amber.Util.PartIso (PartIso)
import qualified Amber.Util.PartIso as PartIso
import Amber.Syntax.Abstract

type AlphaConv = PartIso Name Name

class Unify a where
    unify :: a -> a -> Eff '[Reader AlphaConv, NonDet] ()

    default unify :: Eq a => a -> a -> Eff '[Reader AlphaConv, NonDet] ()
    unify x y
        | x == y = pure ()
        | otherwise = empty

instance Unify Text where

instance Unify a => Unify [a] where
    unify [] [] = pure ()
    unify (x : xs) (y : ys) = unify x y >> unify xs ys
    unify _ _ = empty

instance Unify Name where
    unify x y = asks (PartIso.member x y) >>= \case
        True -> pure ()
        False -> empty

instance Unify Head where
    unify (Local x) (Local y) = unify x y
    unify (Con x) (Con y) = unify x y
    unify (GlobalRec x) (GlobalRec y) = unify x y
    unify (GlobalInd x) (GlobalInd y) = unify x y
    unify _ _ = empty

instance Unify Exp where
    unify (AppE h es) (AppE h' es') = do
        unify h h'
        unify es es'
    unify UnivE UnivE = pure ()
    unify (FunE x e1 e2) (FunE x' e1' e2') = do
        unify e1 e1'
        local (PartIso.insert x x') $ unify e2 e2'
    unify (AmbE e1 e2) e' = unify e1 e' <|> unify e2 e'
    unify e (AmbE e1' e2') = unify e e1' <|> unify e e2'
    unify _ _ = empty
