module Amber.Shell.Panic (
        HasCallStack,
        Excuse(..),
        PanicException(..),
        panic,
        todo,
        impossible,
    ) where

import GHC.Stack
import Control.Exception
import Polysemy
import Data.Typeable
import qualified Data.Text as T

import Amber.Shell.Pretty

data Excuse = Todo | Impossible
    deriving (Show)

data PanicException = PanicException CallStack Excuse Text
    deriving (Typeable, Exception)

panic :: (HasCallStack, Pretty a, PrettyEffects a (Print ': r)) => Excuse -> a -> Sem r b
panic excuse x = do
    (t, _) <- runPrint $ pretty x
    throw $ PanicException callStack excuse t

todo :: (HasCallStack, Pretty a, PrettyEffects a (Print ': r)) => a -> Sem r b
todo = panic Todo

impossible :: (HasCallStack, Pretty a, PrettyEffects a (Print ': r)) => a -> Sem r b
impossible = panic Impossible

instance Show PanicException where
    show (PanicException stack excuse t) = show excuse ++ "\n" ++ T.unpack t ++ "\n" ++ prettyCallStack stack
