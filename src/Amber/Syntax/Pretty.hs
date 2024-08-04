module Amber.Syntax.Pretty (
        Prec(..),
    ) where

import Control.Monad
import Polysemy
import Polysemy.Reader hiding (Local)

import Amber.Shell.Print
import Amber.Shell.Pretty
import Amber.Syntax.Abstract
import Amber.Syntax.Parser (ParseError)

data Prec = TopPrec | AppPrec | BotPrec
    deriving (Show, Eq, Ord, Enum)

instance Pretty Directive where
    type PrettyEffects Directive r = ()

    pretty (RecDec ident ty) = do
        echo ident
        echo " : "
        runReader TopPrec $ pretty ty
        newline
        newline
    pretty (IndDec ident ty) = do
        echo "inductive "
        echo ident
        echo " : "
        runReader TopPrec $ pretty ty
        newline
        newline
    pretty (RecDef ident eqs) = do
        runReader ident $ pretty eqs
        newline
    pretty (IndDef _ cons) = do
        pretty cons
        newline

instance Pretty Equation where
    type PrettyEffects Equation r = Member (Reader Text) r

    pretty (Equation p e) = runReader TopPrec do
        pretty p
        echo " = "
        pretty e
        newline

instance Pretty Constructor where
    type PrettyEffects Constructor r = ()

    pretty (Constructor ident ty) = do
        echo "con "
        echo ident
        echo " : "
        runReader TopPrec $ pretty ty
        newline

instance Pretty Pat where
    type PrettyEffects Pat r = (Member (Reader Text) r, Member (Reader Prec) r)

    pretty SubjectP = ask >>= echo
    pretty (AppP p1 p2) = parens AppPrec do
        withPrec (pred AppPrec) $ pretty p1
        echo " "
        withPrec AppPrec $ pretty p2

instance Pretty SubPat where
    type PrettyEffects SubPat r = Member (Reader Prec) r

    pretty (VarP x) = pretty x
    pretty (ConP ident []) = echo ident
    pretty (ConP ident ps) = parens AppPrec do
        echo ident
        withPrec AppPrec $ pretty ((,) ' ' <$> ps)
    pretty (ForcedP e) = do
        echo "!"
        withPrec BotPrec $ pretty e

instance Pretty Exp where
    type PrettyEffects Exp r = Member (Reader Prec) r

    pretty (AppE h []) = pretty h
    pretty (AppE h es) = parens AppPrec do
        withPrec (pred AppPrec) $ pretty h
        withPrec AppPrec $ pretty ((,) ' ' <$> es)
    pretty UnivE = echo "Type"
    pretty (FunE x e1 e2) = do
        echo "["
        when (givenName x /= "_") do
            pretty x
            echo " : "
        pretty e1
        echo "] "
        pretty e2

instance Pretty Head where
    type PrettyEffects Head r = ()

    pretty (Local x) = pretty x
    pretty (Con ident) = echo ident
    pretty (GlobalRec ident) = echo ident
    pretty (GlobalInd ident) = echo ident

parens :: Prec -> Members '[Print, Reader Prec] r => Sem r () -> Sem r ()
parens p m = do
    reqPrec <- ask
    if reqPrec >= p then withPrec TopPrec $ do
        echo "("
        m
        echo ")"
    else m

withPrec :: Prec -> Member (Reader Prec) r => Sem r a -> Sem r a
withPrec p = local $ const p

instance Pretty ParseError where
    type PrettyEffects ParseError r = ()

    pretty = pretty . show
