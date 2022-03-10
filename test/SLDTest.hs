
import Debug.Trace

import Data.Either 
import Data.Maybe

import App.SubstType

import App.Type
import App.Parser (parse)
import App.Pretty
import App.Substitution
import App.SLDResolution

import App.Unification

parseGoal :: String -> Goal 
parseGoal = (fromRight (error "big error") . parse)

parseProg :: String -> Prog
parseProg = (fromRight (error "big error") . parse)

prop_regression1 = 
    let 
        goal = (Goal [Comb "student_of" [Var (VarName "S"),Comb "peter" []]])
        prog = parseProg "student_of(X,T):-follows(X,C),teaches(T,C).\nfollows(paul,computer_science).\nfollows(paul,expert_systems).\nfollows(maria,ai_techniques).\nteaches(adrian,expert_systems).\nteaches(peter,ai_techniques).\nteaches(peter,computer_science)."

        res = SLDTree (Goal [Comb "student_of" [Var (VarName "S"),Comb "peter" []]]) [(Subst [(VarName "T",Comb "peter" []),(VarName "X",Var (VarName "S"))],SLDTree (Goal [Comb "follows" [Var (VarName "S"),Var (VarName "C")],Comb "teaches" [Comb "peter" [],Var (VarName "C")]]) [(Subst [(VarName "C",Comb "computer_science" []),(VarName "S",Comb "paul" [])],SLDTree (Goal [Comb "teaches" [Comb "peter" [],Comb "computer_science" []]]) []),(Subst [(VarName "C",Comb "expert_systems" []),(VarName "S",Comb "paul" [])],SLDTree (Goal [Comb "teaches" [Comb "peter" [],Comb "expert_systems" []]]) []),(Subst [(VarName "C",Comb "ai_techniques" []),(VarName "S",Comb "maria" [])],SLDTree (Goal [Comb "teaches" [Comb "peter" [],Comb "ai_techniques" []]]) [])])]
    in res == sld prog goal 