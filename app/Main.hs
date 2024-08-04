{-# LANGUAGE DataKinds, OverloadedStrings, TypeApplications #-}
module Main where

import Polysemy
import Polysemy.Reader
import Polysemy.Error
import Polysemy.Fail
import qualified Data.Text.IO as T
import System.Environment

import Amber.Util.Polysemy
import Amber.Shell.Print
import Amber.Shell.Pretty
import Amber.Syntax.Parser
import Amber.Syntax.Resolver
import Amber.Syntax.Pretty ()
import Amber.Typing.Check
import Amber.Typing.Pretty ()
import Amber.Coverage.Check
import Amber.Coverage.Pretty ()

main :: IO ()
main = runFinal
    . embedToFinal @IO
    . failToEmbed @IO
    . printToStdout
    . errorToPrint @ParseError
    . errorToPrint @TypeError
    . errorToPrint @CoverageError
    $ effMain

effMain :: Eff '[Embed IO, Print, Fail, Error ParseError, Error TypeError, Error CoverageError] ()
effMain = do
    [source] <- embed getArgs
    input <- embed $ T.readFile source
    mod <- parseModule source input
    mod' <- resolveModule mod
    pretty mod'
    env <- checkModule mod'
    runReader env $ coverageCheckModule mod'
    echoLn "OK"
    pure ()
