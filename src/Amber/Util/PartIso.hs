module Amber.Util.PartIso (
        PartIso,
        empty,
        insert,
        member,
        diagonal,
    ) where

import Data.Maybe
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as M

data PartIso a b = PartIso (Map a b) (Map b a)
    deriving (Show, Eq, Ord)

empty :: PartIso a b
empty = PartIso M.empty M.empty

insert :: (Ord a, Ord b) => a -> b -> PartIso a b -> PartIso a b
insert x y (PartIso fwd bwd) = PartIso (M.insert x y fwd') (M.insert y x bwd')
    where
        fwd' = maybe fwd (\y' -> M.delete y' fwd) (M.lookup y bwd)
        bwd' = maybe bwd (\x' -> M.delete x' bwd) (M.lookup x fwd)

member :: (Ord a, Ord b) => a -> b -> PartIso a b -> Bool
member x y (PartIso fwd _) = M.lookup x fwd == Just y

diagonal :: Ord a => Set a -> PartIso a a
diagonal xs = PartIso m m
    where
        m = M.fromSet id xs
