{-# LANGUAGE TemplateHaskell #-}
module Amber.Typing.Context (
        GlobalEnv(..),
        recDefs,
        indDefs,
        conTypes,
        LocalEnv(..),
        bindings,
        RecDefDec(..),
        recSignature,
        recStatus,
        IndDefDec(..),
        indSignature,
        indStatus,
        RecStatus(..),
        _RecDeclared,
        _RecDefined,
        IndStatus(..),
        _IndDeclared,
        _IndDefined,
    ) where

import Control.Lens
import Data.Text (Text)
import Data.Map (Map)

import Amber.Syntax.Abstract

data GlobalEnv =
    GlobalEnv {
        _recDefs :: Map Text RecDefDec,
        _indDefs :: Map Text IndDefDec,
        _conTypes :: Map Text Text
    }
    deriving (Show)

data LocalEnv =
    LocalEnv {
        _bindings :: Map Name Exp
    }
    deriving (Show)

data RecDefDec =
    RecDefDec {
        _recSignature :: Exp,
        _recStatus :: RecStatus
    }
    deriving (Show)

data IndDefDec =
    IndDefDec {
        _indSignature :: Exp,
        _indStatus :: IndStatus
    }
    deriving (Show)

data RecStatus = RecDeclared | RecDefined [Equation]
    deriving (Show)

data IndStatus = IndDeclared | IndDefined [Constructor]
    deriving (Show)

makeLenses ''GlobalEnv
makeLenses ''LocalEnv
makeLenses ''RecDefDec
makeLenses ''IndDefDec
makePrisms ''RecStatus
makePrisms ''IndStatus

instance Semigroup GlobalEnv where
    GlobalEnv recDefs1 indDefs1 conTypes1 <> GlobalEnv recDefs2 indDefs2 conTypes2 = GlobalEnv (recDefs1 <> recDefs2) (indDefs1 <> indDefs2) (conTypes1 <> conTypes2)

instance Monoid GlobalEnv where
    mempty = GlobalEnv mempty mempty mempty

instance Semigroup LocalEnv where
    LocalEnv bindings1 <> LocalEnv bindings2 = LocalEnv (bindings1 <> bindings2)

instance Monoid LocalEnv where
    mempty = LocalEnv mempty
