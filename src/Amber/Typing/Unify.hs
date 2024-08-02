module Amber.Typing.Unify (
        Unify(..),
        unifyWith,
    ) where

import Control.Monad.Reader
import Control.Monad.Except
import Data.Text (Text)

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
    unify :: Monad m => a -> a -> ReaderT AlphaConv (ExceptT UnifyError m) ()

    default unify :: (Eq a, Monad m) => a -> a -> ReaderT AlphaConv (ExceptT UnifyError m) ()
    unify x y
        | x == y = pure ()
        | otherwise = different x y

unifyWith :: (MonadError e m, Unify a) => (UnifyError -> e) -> AlphaConv -> a -> a -> m ()
unifyWith f conv x y = modifyError f $ unify x y `runReaderT` conv

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

different :: Monad m => a -> a -> ReaderT AlphaConv (ExceptT UnifyError m) b
different _ _ = throwError ()
