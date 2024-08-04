{-# LANGUAGE TemplateHaskell, RecursiveDo #-}
module Amber.Syntax.Name (
        Text,
        Name,
        NameGen,
        runNameGen,
        givenName,
        uniqueName,
        canonicalName,
        distinctName,
        newName,
    ) where

import Polysemy
import Polysemy.State
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as S

import Amber.Shell.Print
import Amber.Shell.Pretty

data Name = Name Index Text
    deriving (Eq, Ord)

newtype Index = Index Integer
    deriving (Eq, Ord, Enum)
    deriving newtype (Show)

instance Semigroup Index where
    Index i <> Index j = Index $ max i j

instance Monoid Index where
    mempty = Index 0

data NameGen m a where
    NewName :: Text -> NameGen m Name

makeSem ''NameGen

runNameGen :: Set Name -> Sem (NameGen ': r) a -> Sem r a
runNameGen names = evalState names . reinterpret \case
    NewName x -> do
        names <- get
        let name = distinctName (canonicalName x) names
        put $ S.insert name names
        pure name

givenName :: Name -> Text
givenName (Name _ x) = x

uniqueName :: Name -> Text
uniqueName (Name (Index 0) x) = x
uniqueName (Name (Index i) x) = x <> T.pack (map subscript (show i))

subscript :: Char -> Char
subscript '0' = '₀'
subscript '1' = '₁'
subscript '2' = '₂'
subscript '3' = '₃'
subscript '4' = '₄'
subscript '5' = '₅'
subscript '6' = '₆'
subscript '7' = '₇'
subscript '8' = '₈'
subscript '9' = '₉'
subscript _ = undefined

canonicalName :: Text -> Name
canonicalName = Name (Index 0)

distinctName :: Foldable t => Name -> t Name -> Name
distinctName (Name i x) names = Name (foldMap (\(Name j y) -> if y == x then succ j else i) names) x

instance Show Name where
    show (Name i x) = T.unpack x ++ "_" ++ show i

instance Pretty Name where
    type PrettyEffects Name r = ()

    pretty = echo . uniqueName
