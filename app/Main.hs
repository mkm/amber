{-# LANGUAGE OverloadedStrings, TypeApplications #-}
module Main where

import Polysemy
import Polysemy.Reader
import Polysemy.Error
import Polysemy.Fixpoint
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment
import System.Exit

import Amber.Syntax.Name
import Amber.Syntax.Parser
import Amber.Syntax.Resolver
import Amber.Syntax.Printer
import Amber.Typing.Check
import Amber.Coverage.Check

main :: IO ()
main = do
    [source] <- getArgs
    input <- T.readFile source
    mod <- case parseModule source input of
        Left err -> print err >> exitFailure
        Right mod -> pure mod
    let mod' = run $ resolveModule mod
    T.putStr $ printModule mod'
    let checkResult = run . runError $ checkModule mod'
    case checkResult of
        Left err -> T.putStrLn $ printError err
        Right env ->
            case run . runError . runReader env $ coverageCheckModule mod' of
                Left err -> T.putStrLn $ printCoverageError err
                Right () -> putStrLn "OK"

printError :: TypeError -> Text
printError (ExpUnifyError e1 e2) =
    printExp e1 <> " ≠ " <> printExp e2
printError (NotUnivError e) =
    printExp e <> " is not a universe"
printError (InEquation ident eq err) =
    printError err <> "\n  while checking equation\n" <> printEquation ident eq
printError (InPat ident p err) =
    printError err <> "\n  while checking pattern\n" <> printPat ident p
printError (InExp e err) =
    printError err <> "\n  while checking expression\n" <> printExp e
printError (InExpAt ty e err) =
    printError err <> "\n  while checking expression\n" <> printExp e <> " : " <> printExp ty
printError (InExpApp ty es err) =
    printError err <> "\n  while checking application\n(_ : " <> printExp ty <> ")" <> mconcat ["\n    " <> printExp e | e <- es]
printError (InHead h err) =
    printError err <> "\n  while checking head expression\n" <> printHead h
printError err =
    T.pack $ show err

printCoverageError :: CoverageError -> Text
printCoverageError (MissingPatternsError ident ps) =
    ident <> " does not cover all cases" <> mconcat ["\n" <> printPat ident p | p <- ps]
