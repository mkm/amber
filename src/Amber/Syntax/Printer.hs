{-# LANGUAGE TemplateHaskell #-}
module Amber.Syntax.Printer (
        printModule,
        printEquation,
        printPat,
        printExp,
        printHead,
    ) where

import Prelude hiding (exp, head)
import Control.Lens hiding (Context)
import Control.Monad
import Polysemy
import Polysemy.Reader hiding (Local)
import Polysemy.Writer
import Data.Foldable
import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as S

import Amber.Util.Polysemy
import qualified Amber.Util.PartIso as PartIso
import Amber.Syntax.Abstract
import Amber.Analysis.Occurrence

type Print a = Eff '[Reader Context, Writer Text] a

data Context =
    Context {
        _prec :: Prec,
        _vars :: Set Name
    }

data Prec = TopPrec | AppPrec | BotPrec
    deriving (Show, Eq, Ord, Enum)

makeLenses ''Context

printModule :: Module -> Text
printModule mod = fst . run . runWriter . runReader emptyContext $ module' mod

printEquation :: Text -> Equation -> Text
printEquation ident eq = fst . run . runWriter . runReader emptyContext $ equation ident eq

printPat :: Text -> Pat -> Text
printPat ident p = fst . run . runWriter . runReader emptyContext $ pat ident p

printExp :: Exp -> Text
printExp e = fst . run . runWriter . runReader emptyContext $ exp e

printHead :: Head -> Text
printHead h = fst . run . runWriter . runReader emptyContext $ head h

emptyContext :: Context
emptyContext = Context { _prec = TopPrec, _vars = S.empty }

module' :: Module -> Print ()
module' = mapM_ directive

directive :: Directive -> Print ()
directive (RecDec ident ty) = do
    emit ident
    colon
    exp ty
    newline
    newline
directive (IndDec ident ty) = do
    emit "inductive "
    emit ident
    colon
    exp ty
    newline
    newline
directive (RecDef ident eqs) = do
    traverse_ (equation ident) eqs
    newline
directive (IndDef _ cons) = do
    traverse_ constructor cons
    newline

equation :: Text -> Equation -> Print ()
equation ident (Equation p e) = do
    pat ident p
    equal
    exp e
    newline

constructor :: Constructor -> Print ()
constructor (Constructor ident ty) = do
    emit "con "
    emit ident
    colon
    exp ty
    newline

pat :: Text -> Pat -> Print ()
pat ident SubjectP = emit ident
pat ident (AppP p1 p2) = parens AppPrec $ do
    withPrec (pred AppPrec) $ pat ident p1
    emit " "
    withPrec AppPrec $ subPat p2

subPat :: SubPat -> Print ()
subPat (VarP x) = name x
subPat (ConP ident []) = emit ident
subPat (ConP ident ps) = parens AppPrec do
    emit ident
    withPrec AppPrec . forM_ ps $ \p -> do
        emit " "
        subPat p
subPat (ForcedP e) = do
    emit "!"
    withPrec BotPrec $ exp e

exp :: Exp -> Print ()
exp (AppE h []) = head h
exp (AppE h es) = parens AppPrec do
    withPrec (pred AppPrec) $ head h
    withPrec AppPrec . forM_ es $ \e -> do
        emit " "
        exp e
exp UnivE = emit "Type"
exp (FunE x e1 e2) = do
    emit "["
    conv <- PartIso.diagonal . S.insert x <$> asks (view vars)
    when (occursIn conv x e2) $ do
        name x
        colon
    exp e1
    emit "] "
    local (vars %~ S.insert x) $ exp e2

head :: Head -> Print ()
head (Local x) = name x
head (Con ident) = emit ident
head (GlobalRec ident) = emit ident
head (GlobalInd ident) = emit ident

name :: Name -> Print ()
name = emit . uniqueName

parens :: Prec -> Print () -> Print ()
parens p m = do
    reqPrec <- asks $ view prec
    if reqPrec >= p then withPrec TopPrec $ do
        emit "("
        m
        emit ")"
    else m

withPrec :: Prec -> Member (Reader Context) r => Sem r a -> Sem r a
withPrec p = local $ prec .~ p

colon :: Print ()
colon = emit " : "

equal :: Print ()
equal = emit " = "

newline :: Print ()
newline = emit "\n"

emit :: Text -> Print ()
emit = tell
