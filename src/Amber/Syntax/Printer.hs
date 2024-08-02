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
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Foldable
import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as S

import qualified Amber.Util.PartIso as PartIso
import Amber.Syntax.Abstract
import Amber.Analysis.Occurrence

type Print m = ReaderT Context (WriterT Text m)

data Context =
    Context {
        _prec :: Prec,
        _vars :: Set Name
    }

data Prec = TopPrec | AppPrec | BotPrec
    deriving (Show, Eq, Ord, Enum)

makeLenses ''Context

printModule :: Monad m => Module -> m Text
printModule mod = execWriterT $ module' mod `runReaderT` emptyContext

printEquation :: Monad m => Text -> Equation -> m Text
printEquation ident eq = execWriterT $ equation ident eq `runReaderT` emptyContext

printPat :: Monad m => Text -> Pat -> m Text
printPat ident p = execWriterT $ pat ident p `runReaderT` emptyContext

printExp :: Monad m => Exp -> m Text
printExp e = execWriterT $ exp e `runReaderT` emptyContext

printHead :: Monad m => Head -> m Text
printHead h = execWriterT $ head h `runReaderT` emptyContext

emptyContext :: Context
emptyContext = Context { _prec = TopPrec, _vars = S.empty }

module' :: Monad m => Module -> Print m ()
module' = mapM_ directive

directive :: Monad m => Directive -> Print m ()
directive (RecDec ident ty) = do
    tell ident
    colon
    exp ty
    newline
    newline
directive (IndDec ident ty) = do
    tell "inductive "
    tell ident
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

equation :: Monad m => Text -> Equation -> Print m ()
equation ident (Equation p e) = do
    pat ident p
    equal
    exp e
    newline

constructor :: Monad m => Constructor -> Print m ()
constructor (Constructor ident ty) = do
    tell "con "
    tell ident
    colon
    exp ty
    newline

pat :: Monad m => Text -> Pat -> Print m ()
pat ident SubjectP = tell ident
pat ident (AppP p1 p2) = parens AppPrec $ do
    withPrec (pred AppPrec) $ pat ident p1
    tell " "
    withPrec AppPrec $ subPat p2

subPat :: Monad m => SubPat -> Print m ()
subPat (VarP x) = name x
subPat (ConP ident []) = tell ident
subPat (ConP ident ps) = parens AppPrec do
    tell ident
    withPrec AppPrec . forM_ ps $ \p -> do
        tell " "
        subPat p
subPat (ForcedP e) = do
    tell "!"
    withPrec BotPrec $ exp e

exp :: Monad m => Exp -> Print m ()
exp (AppE h []) = head h
exp (AppE h es) = parens AppPrec do
    withPrec (pred AppPrec) $ head h
    withPrec AppPrec . forM_ es $ \e -> do
        tell " "
        exp e
exp UnivE = tell "Type"
exp (FunE x e1 e2) = do
    tell "["
    conv <- PartIso.diagonal . S.insert x <$> view vars
    when (occursIn conv x e2) $ do
        name x
        colon
    exp e1
    tell "] "
    local (vars %~ S.insert x) $ exp e2

head :: Monad m => Head -> Print m ()
head (Local x) = name x
head (Con ident) = tell ident
head (GlobalRec ident) = tell ident
head (GlobalInd ident) = tell ident

name :: Monad m => Name -> Print m ()
name = tell . uniqueName

parens :: Monad m => Prec -> Print m () -> Print m ()
parens p m = do
    reqPrec <- view prec
    if reqPrec >= p then withPrec TopPrec $ do
        tell "("
        m
        tell ")"
    else m

withPrec :: Monad m => Prec -> Print m () -> Print m ()
withPrec p = local $ prec .~ p

colon :: Monad m => Print m ()
colon = tell " : "

equal :: Monad m => Print m ()
equal = tell " = "

newline :: Monad m => Print m ()
newline = tell "\n"
