module Amber.Analysis.Occurrence (
        Occur(..),
        occursIn,
    ) where

import Control.Lens
import Control.Monad
import Data.Text (Text)

import qualified Amber.Util.PartIso as PartIso
import Amber.Syntax.Abstract
import Amber.Syntax.Equivalence

class Occur s a where
    occurrences :: AlphaConv -> a -> Traversal' s a

occursIn :: Occur s a => AlphaConv -> a -> s -> Bool
occursIn conv a = has $ occurrences conv a

instance Occur Text Text where
    occurrences _ x f y
        | x == y = f y
        | otherwise = pure y

instance Occur Name Name where
    occurrences conv x f y
        | equiv x y conv = f y
        | otherwise = pure y

instance Occur Head Head where
    occurrences conv (Local x') f h@(Local x)
        | equiv x' x conv = f h
    occurrences _ (GlobalRec x') f h@(GlobalRec x)
        | x' == x = f h
    occurrences _ (GlobalInd x') f h@(GlobalInd x)
        | x' == x = f h
    occurrences _ _ _ h = pure h

instance Occur Exp Exp where
    occurrences conv e' f e
        | equiv e' e conv = f e
    occurrences conv (AppE h' es') f (AppE h es)
        | equiv h' h conv && prefix == length es' = (`app` drop prefix es) <$> f (AppE h (take prefix es))
        where
            prefix = length . takeWhile id $ zipWithM equiv es' es conv
    occurrences conv e' f (AppE h es) = AppE h <$> traverse (occurrences conv e' f) es
    occurrences _ _ _ UnivE = pure UnivE
    occurrences conv e' f (FunE x e1 e2) = FunE x <$> occurrences conv' e' f e1 <*> occurrences conv' e' f e2
        where
            conv' = PartIso.insert x x conv

instance Occur Exp Head where
    occurrences conv h' f (AppE h es) = AppE <$> occurrences conv h' f h <*> traverse (occurrences conv h' f) es

instance Occur Head Name where
    occurrences conv x' f (Local x)
        | equiv x' x conv = Local <$> f x
    occurrences _ _ _ h = pure h

instance Occur Exp Name where
    occurrences conv x' f (AppE h es) = AppE <$> occurrences conv x' f h <*> traverse (occurrences conv x' f) es
    occurrences _ _ _ UnivE = pure UnivE
    occurrences conv x' f (FunE x e1 e2) = FunE x <$> occurrences conv x' f e1 <*> occurrences (PartIso.insert x x conv) x' f e2
