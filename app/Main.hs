{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Identity
import Control.Monad.Except
import Polysemy
import Polysemy.Reader
import Polysemy.Error
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
    let mod' = runIdentity $ resolveModule mod
    printModule mod' >>= T.putStr
    checkResult <- runExceptT (checkModule mod')
    case checkResult of
        Left err -> printError err >>= T.putStrLn
        Right env ->
            case run . runError . runReader env $ coverageCheckModule mod' of
                Left err -> printCoverageError err >>= T.putStrLn
                Right () -> putStrLn "OK"

printError :: Monad m => TypeError -> m Text
printError (ExpUnifyError e1 e2) = do
    s1 <- printExp e1
    s2 <- printExp e2
    pure $ s1 <> " ≠ " <> s2
printError (NotUnivError e) = do
    s <- printExp e
    pure $ s <> " is not a universe"
printError (InEquation ident eq err) = do
    sErr <- printError err
    sEq <- printEquation ident eq
    pure $ sErr <> "\n  while checking equation\n" <> sEq
printError (InPat ident p err) = do
    sErr <- printError err
    sP <- printPat ident p
    pure $ sErr <> "\n  while checking pattern\n" <> sP
printError (InExp e err) = do
    sErr <- printError err
    sE <- printExp e
    pure $ sErr <> "\n  while checking expression\n" <> sE
printError (InExpAt ty e err) = do
    sErr <- printError err
    sTy <- printExp ty
    sE <- printExp e
    pure $ sErr <> "\n  while checking expression\n" <> sE <> " : " <> sTy
printError (InExpApp ty es err) = do
    sErr <- printError err
    sTy <- printExp ty
    sEs <- traverse printExp es
    pure $ sErr <> "\n  while checking application\n(_ : " <> sTy <> ")" <> mconcat ["\n    " <> sE | sE <- sEs]
printError (InHead h err) = do
    sErr <- printError err
    sH <- printHead h
    pure $ sErr <> "\n  while checking head expression\n" <> sH
printError err = pure . T.pack $ show err

printCoverageError :: Monad m => CoverageError -> m Text
printCoverageError (MissingPatternsError ident ps) = do
    sPs <- traverse (printPat ident) ps
    pure $ ident <> " does not cover all cases" <> mconcat ["\n" <> sP | sP <- sPs]
