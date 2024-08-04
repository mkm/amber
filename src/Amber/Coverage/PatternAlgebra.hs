module Amber.Coverage.PatternAlgebra (
        PatSystem,
        union,
        intersection,
        complement,
    ) where

import Control.Lens
import Control.Applicative
import Control.Monad
import Polysemy
import Polysemy.Reader
import Polysemy.NonDet
import qualified Data.Set as S

import Amber.Shell.Panic
import Amber.Syntax.Abstract
import Amber.Syntax.Free
import Amber.Syntax.Pretty
import Amber.Typing.Context

type PatSystem = [Pat]

union :: PatSystem -> PatSystem -> PatSystem
union = (++)

type E a = forall r. Members [NonDet, NameGen] r => Sem r a

intersection :: PatSystem -> PatSystem -> PatSystem
intersection ps1 ps2 = do
    p1 <- ps1
    p2 <- ps2
    go p1 p2
    where
        go :: Pat -> Pat -> PatSystem
        go p p' = run . runNonDet . runNameGen (patFree p `S.union` patFree p') $ pat p p'

        pat :: Pat -> Pat -> E Pat
        pat p1 p2 = merge SubjectP (unfoldPat p1) (unfoldPat p2)

        merge :: Pat -> [SubPat] -> [SubPat] -> E Pat
        merge p0 [] ps' = pure $ foldPat p0 ps'
        merge p0 ps [] = pure $ foldPat p0 ps
        merge p0 (p : ps) (p' : ps') = do
            p'' <- subPat p p'
            merge (AppP p0 p'') ps ps'

        subPat :: SubPat -> SubPat -> E SubPat
        subPat (ForcedP _) p' = pure p'
        subPat p (ForcedP _) = pure p
        subPat (VarP _) p' = pure p'
        subPat p (VarP _) = pure p
        subPat (ConP ident ps) (ConP ident' ps')
            | ident == ident' = ConP ident <$> zipWithM subPat ps ps'
            | otherwise = empty

        unfoldPat :: Pat -> [SubPat]
        unfoldPat SubjectP = []
        unfoldPat (AppP p1 p2) = unfoldPat p1 ++ [p2]

        foldPat :: Pat -> [SubPat] -> Pat
        foldPat p0 [] = p0
        foldPat p0 (p : ps) = foldPat (AppP p0 p) ps

complement :: GlobalEnv -> Pat -> PatSystem
complement env = go
    where
        go :: Pat -> PatSystem
        go p = run . runNonDet . runNameGen (patFree p) $ pat p

        pat :: Pat -> E Pat
        pat SubjectP = empty
        pat (AppP p1 p2) =
            do
                p1' <- pat p1
                x <- newName "?"
                pure $ AppP p1' (VarP x)
            <|>
            do
                p1' <- genPat p1
                p2' <- subPat p2
                pure $ AppP p1' p2'

        subPat :: SubPat -> E SubPat
        subPat (VarP _) = empty
        subPat (ConP ident ps) = do
            tyName <- maybe (impossible ident) pure $ env ^. conTypes . at ident
            let cons = env ^. indDefs . at tyName . _Just . indStatus . _IndDefined
            ConP ident <$> conArgs ps <|> asum [ConP ident' <$> genCons ty | Constructor ident' ty <- cons, ident' /= ident]
        subPat (ForcedP _) = empty

        conArgs :: [SubPat] -> E [SubPat]
        conArgs [] = empty
        conArgs (p : ps) = (:) <$> subPat p <*> pure ps <|> (:) p <$> conArgs ps

        genPat :: Pat -> E Pat
        genPat SubjectP = pure SubjectP
        genPat (AppP p1 _) = AppP <$> genPat p1 <*> (VarP <$> newName "?")

        genCons :: Exp -> E [SubPat]
        genCons (FunE _ _ ty) = (:) <$> (VarP <$> newName "?") <*> genCons ty
        genCons (AppE _ _) = pure []
        genCons e = runReader TopPrec $ impossible e
