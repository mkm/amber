module Amber.Coverage.Pretty (
    ) where

import Polysemy.Reader

import Amber.Shell.Print
import Amber.Shell.Pretty
import Amber.Syntax.Pretty
import Amber.Coverage.Check

instance Pretty CoverageError where
    type PrettyEffects CoverageError r = ()

    pretty (MissingPatternsError ident ps) = do
        echo ident
        echo " does not cover all cases"
        runReader ident . runReader TopPrec . pretty $ ((,) '\n' <$> ps)
        newline
