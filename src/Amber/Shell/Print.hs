{-# LANGUAGE TemplateHaskell #-}
module Amber.Shell.Print (
        Text,
        Print(..),
        echo,
        newline,
        echoLn,
        runPrint,
        printToStdout,
    ) where

import Polysemy
import Polysemy.Writer
import Data.Bifunctor
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text.Internal.StrictBuilder as TB

import Amber.Util.Polysemy

data Print m a where
    Echo :: Text -> Print m ()

makeSem ''Print

newline :: Eff '[Print] ()
newline = echo "\n"

echoLn :: Text -> Eff '[Print] ()
echoLn t = echo t >> newline

runPrint :: Sem (Print ': r) a -> Sem r (Text, a)
runPrint = fmap (first TB.toText) . runWriter . reinterpret \case
    Echo t -> tell (TB.fromText t)

printToStdout :: Member (Embed IO) r => Sem (Print ': r) a -> Sem r a
printToStdout = interpret \case
    Echo t -> embed $ T.putStr t
