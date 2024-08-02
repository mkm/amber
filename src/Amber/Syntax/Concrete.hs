module Amber.Syntax.Concrete (
        Module,
        Directive(..),
        Pat(..),
        Exp(..),
        TokenKind(..),
        Token(..),
        Keyword(..),
    ) where

import Data.Text (Text)
import Text.Parsec (SourcePos)

type Module = [Directive]

data Directive
    = RecDec Text Exp
    | IndDec Text Exp
    | Equation Pat Exp
    | Constructor Text Exp
    deriving (Show, Read)

data Pat
    = VarP Text
    | WildP
    | ForcedP Exp
    | AppP Pat Pat
    deriving (Show, Read)

data Exp
    = VarE Text
    -- | LamE (Maybe Text) Exp Exp
    | AppE Exp Exp
    | UnivE
    | FunE (Maybe Text) Exp Exp
    deriving (Show, Read)

data Token = Token (Maybe Int) SourcePos TokenKind
    deriving (Show)

data TokenKind
    = Word Text
    | Keyword Keyword
    deriving (Show, Eq)

data Keyword
    = LambdaK
    | ColonK
    | EqualK
    | ArrowK
    | BangK
    | UnderscoreK
    | TypeK
    | InductiveK
    | ConK
    | LeftParenK
    | RightParenK
    | LeftBracketK
    | RightBracketK
    deriving (Show, Eq)
