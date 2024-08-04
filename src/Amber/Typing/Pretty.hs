module Amber.Typing.Pretty (
    ) where

import Polysemy.Reader

import Amber.Shell.Print
import Amber.Shell.Pretty
import Amber.Syntax.Pretty
import Amber.Typing.Check

instance Pretty TypeError where
    type PrettyEffects TypeError r = ()

    pretty (ExpUnifyError e1 e2) = do
        runReader TopPrec $ pretty e1
        echo " ≠ "
        runReader TopPrec $ pretty e2
    pretty (NotFunError ty) = do
        runReader TopPrec $ pretty ty
        echo " is not a function type"
    pretty (NotUnivError e) = do
        runReader TopPrec $ pretty e
        echo " is not a universe"
    pretty (InEquation ident eq err) = do
        prettyLn err
        echoLn "  while checking equation"
        runReader ident $ pretty eq
    pretty (InPat ident p err) = do
        prettyLn err
        echoLn "  while checking pattern"
        runReader ident . runReader TopPrec $ pretty p
    pretty (InExp e err) = do
        prettyLn err
        echoLn "  while checking expression"
        runReader TopPrec $ pretty e
    pretty (InExpAt ty e err) = do
        prettyLn err
        echoLn "  while checking expression"
        runReader TopPrec $ pretty e
        echo " : "
        runReader TopPrec $ pretty ty
    pretty (InExpApp ty es err) = do
        prettyLn err
        echoLn "  while checking application"
        echo "(_ : "
        runReader TopPrec $ pretty ty
        echo ")"
        runReader BotPrec $ pretty ((,) ' ' <$> es)
        -- mconcat ["\n    " <> pretty e | e <- es]
    pretty (InHead h err) = do
        prettyLn err
        echoLn "  while checking head expression"
        pretty h
    pretty err = pretty $ show err
