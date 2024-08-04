module Amber.Shell.Pretty (
        module Amber.Shell.Print,
        Pretty(..),
        Intercalate(..),
        prettyLn,
        errorToPrint,
    ) where

import Polysemy
import Polysemy.Error
import Data.Kind
import Data.Foldable
import qualified Data.Text as T

import Amber.Shell.Print

class Pretty a where
    type PrettyEffects a (r :: EffectRow) :: Constraint

    pretty :: (Member Print r, PrettyEffects a r) => a -> Sem r ()

instance Pretty Text where
    type PrettyEffects Text r = ()

    pretty = echo

instance Pretty Char where
    type PrettyEffects Char r = ()

    pretty = echo . T.singleton

instance Pretty a => Pretty [a] where
    type PrettyEffects [a] r = PrettyEffects a r

    pretty = traverse_ pretty

instance (Pretty a, Pretty b) => Pretty (a, b) where
    type PrettyEffects (a, b) r = (PrettyEffects a r, PrettyEffects b r)

    pretty (a, b) = do
        pretty a
        pretty b

instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
    type PrettyEffects (a, b, c) r = (PrettyEffects a r, PrettyEffects b r, PrettyEffects c r)

    pretty (a, b, c) = do
        pretty a
        pretty b
        pretty c

data Intercalate a b = Intercalate a [b]

instance (Pretty a, Pretty b) => Pretty (Intercalate a b) where
    type PrettyEffects (Intercalate a b) r = (PrettyEffects a r, PrettyEffects b r)

    pretty (Intercalate _ []) = pure ()
    pretty (Intercalate sep (x : xs)) = do
        pretty x
        pretty ((,) sep <$> xs)

prettyLn :: (Pretty a, Member Print r, PrettyEffects a r) => a -> Sem r ()
prettyLn x = pretty x >> newline

errorToPrint :: forall e r. (Pretty e, PrettyEffects e r, Member Print r) => Sem (Error e ': r) () -> Sem r ()
errorToPrint m = runError m >>= \case
    Left err -> pretty err
    Right () -> pure ()
