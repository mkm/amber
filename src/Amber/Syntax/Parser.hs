module Amber.Syntax.Parser (
        ParseError,
        parseExp,
        parseModule,
        tokenise,
    ) where

import Prelude hiding (exp)
import Control.Monad
import Polysemy.Error hiding (try)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Text.Parsec hiding (token)
import qualified Text.Parsec as Parsec

import Amber.Util.Polysemy
import Amber.Syntax.Concrete

type Tok = Parsec Text (Maybe Int)
type Par = Parsec [Token] IndentState

type IndentState = (Int, BlockState)
data BlockState = NewBlock | WithinBlock
    deriving (Show)

parseExp :: SourceName -> Text -> Either ParseError Exp
parseExp source =
    tokenise source >=> runParser (exp <* eof) (-1, NewBlock) source

parseModule :: SourceName -> Text -> Eff '[Error ParseError] Module
parseModule source input =
    fromEither $ tokenise source input >>= runParser (many directive <* eof) (-1, WithinBlock) source

tokenise :: SourceName -> Text -> Either ParseError [Token]
tokenise = runParser (blanks *> many token <* blanks <* eof) (Just 0)

token :: Tok Token
token = Token <$> getState <*> getPosition <*> (word <|> separator)

word :: Tok TokenKind
word = do
    chars <- many1 symbol
    setState Nothing
    blanks
    case M.lookup chars keywords of
        Nothing -> pure $ Word (T.pack chars)
        Just kw -> pure $ Keyword kw

symbol :: Tok Char
symbol = noneOf $ " \n" ++ map fst separators

keywords :: Map String Keyword
keywords =
    M.fromList
        [ ("λ", LambdaK)
        , ("=", EqualK)
        , ("→", ArrowK)
        , ("_", UnderscoreK)
        , ("Type", TypeK)
        , ("inductive", InductiveK)
        , ("con", ConK)
        ]

separator :: Tok TokenKind
separator = do
    kw <- choice [Keyword kw <$ char c | (c, kw) <- separators]
    setState Nothing
    blanks
    pure kw

separators :: [(Char, Keyword)]
separators =
    [ (':', ColonK)
    , ('!', BangK)
    , ('(', LeftParenK)
    , (')', RightParenK)
    , ('[', LeftBracketK)
    , (']', RightBracketK)
    ]

blanks :: Tok ()
blanks = void $ many blank

blank :: Tok ()
blank =
    (newline >> setState (Just 0)) <|>
    (char ' ' >> modifyState (fmap (+ 1))) <|>
    (string "--" >> manyTill anyChar newline >> pure ())

directive :: Par Directive
directive = recDec <|> indDec <|> equation <|> constructor

recDec :: Par Directive
recDec = try . block $ do
    name <- ident
    colon
    ty <- exp
    pure $ RecDec name ty

indDec :: Par Directive
indDec = block $ do
    inductive
    name <- ident
    colon
    ty <- exp
    pure $ IndDec name ty

equation :: Par Directive
equation = try . block $ do
    p <- pat
    equal
    e <- exp
    pure $ Equation p e

constructor :: Par Directive
constructor = block $ do
    con
    name <- ident
    colon
    ty <- exp
    pure $ Constructor name ty

exp :: Par Exp
exp = funExp

funExp :: Par Exp
funExp = do
    doms <- many domain
    codom <- appExp
    pure $ foldr (\(name, dom) -> FunE name dom) codom doms

domain :: Par (Maybe Text, Exp)
domain = between leftBracket rightBracket $ do
    name <- optionMaybe (try $ ident <* colon)
    ty <- exp
    pure (name, ty)

appExp :: Par Exp
appExp = foldl1 AppE <$> many1 simpleExp

simpleExp :: Par Exp
simpleExp = varExp <|> univExp <|> parenExp

varExp :: Par Exp
varExp = VarE <$> ident

univExp :: Par Exp
univExp = UnivE <$ matchKind (Keyword TypeK)

parenExp :: Par Exp
parenExp = between leftParen rightParen exp

pat :: Par Pat
pat = appPat

appPat :: Par Pat
appPat = foldl1 AppP <$> many1 simplePat

simplePat :: Par Pat
simplePat = varPat <|> wildPat <|> forcedPat <|> parenPat

varPat :: Par Pat
varPat = VarP <$> ident

wildPat :: Par Pat
wildPat = WildP <$ underscore

forcedPat :: Par Pat
forcedPat = do
    bang
    ForcedP <$> simpleExp

parenPat :: Par Pat
parenPat = between leftParen rightParen pat

ident :: Par Text
ident = match m
    where
        m (Word t) = Just t
        m _ = Nothing

colon :: Par ()
colon = matchKind (Keyword ColonK)

equal :: Par ()
equal = matchKind (Keyword EqualK)

bang :: Par ()
bang = matchKind (Keyword BangK)

underscore :: Par ()
underscore = matchKind (Keyword UnderscoreK)

inductive :: Par ()
inductive = matchKind (Keyword InductiveK)

con :: Par ()
con = matchKind (Keyword ConK)

leftParen :: Par ()
leftParen = matchKind (Keyword LeftParenK)

rightParen :: Par ()
rightParen = matchKind (Keyword RightParenK)

leftBracket :: Par ()
leftBracket = matchKind (Keyword LeftBracketK)

rightBracket :: Par ()
rightBracket = matchKind (Keyword RightBracketK)

block :: Par a -> Par a
block p = do
    oldState@(oldIndent, WithinBlock) <- getState
    putState (oldIndent, NewBlock)
    x <- p
    putState oldState
    pure x

matchKind :: TokenKind -> Par ()
matchKind k = match $ \k' -> if k == k' then Just () else Nothing

match :: (TokenKind -> Maybe a) -> Par a
match f = try $ Parsec.token showToken tokenPos test >>= \x -> getState >>= uncurry adjust x
    where
        showToken (Token _ _ k) = show k
        tokenPos (Token _ pos _) = pos
        test (Token indent _ k) = (,) indent <$> f k
        adjust (Just indent) k (blockIndent, NewBlock)
            | indent > blockIndent = k <$ putState (indent, WithinBlock)
        adjust _ _ (_, NewBlock) = fail "block is not indented"
        adjust (Just indent) _ (blockIndent, WithinBlock)
            | indent <= blockIndent = fail "anti-indented"
        adjust _ k (_, WithinBlock) = pure k
