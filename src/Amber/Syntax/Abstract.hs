module Amber.Syntax.Abstract (
        module Amber.Syntax.Name,
        Module,
        Directive(..),
        Equation(..),
        Constructor(..),
        Pat(..),
        SubPat(..),
        Exp(..),
        Head(..),
        app,
        app1,
        appSP,
        appSP1,
    ) where

import Amber.Shell.Panic
import Amber.Syntax.Name

type Module = [Directive]

data Directive
    = RecDec Text Exp
    | IndDec Text Exp
    | RecDef Text [Equation]
    | IndDef Text [Constructor]
    deriving (Show)

data Equation = Equation Pat Exp
    deriving (Show)

data Constructor = Constructor Text Exp
    deriving (Show)

data Pat
    = SubjectP
    | AppP Pat SubPat
    deriving (Show)

data SubPat
    = VarP Name
    | ConP Text [SubPat]
    | ForcedP Exp
    deriving (Show)

data Exp
    = AppE Head [Exp]
    | UnivE
    | FunE Name Exp Exp
    | AmbE Exp Exp
    deriving (Show)

data Head
    = Local Name
    | Con Text
    | GlobalRec Text
    | GlobalInd Text
    deriving (Show)

app :: HasCallStack => Exp -> [Exp] -> Exp
app (AppE h es) es' = AppE h (es ++ es')
app e [] = e
app _ _ = undefined

app1 :: Exp -> Exp -> Exp
app1 e1 e2 = app e1 [e2]

appSP :: HasCallStack => SubPat -> [SubPat] -> SubPat
appSP (ConP ident ps) ps' = ConP ident (ps ++ ps')
appSP _ _ = undefined

appSP1 :: SubPat -> SubPat -> SubPat
appSP1 p1 p2 = appSP p1 [p2]
