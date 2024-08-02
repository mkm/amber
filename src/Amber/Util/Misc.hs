module Amber.Util.Misc (
        (<&&>),
        compose,
    ) where

infixr 3 <&&>
(<&&>) :: Monad m => m Bool -> m Bool -> m Bool
a <&&> b = a >>= \case
    False -> pure False
    True -> b

compose :: Foldable t => t (a -> a) -> a -> a
compose = foldr (.) id
