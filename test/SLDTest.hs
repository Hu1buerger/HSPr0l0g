
import Data.Either 
import Data.Maybe

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

goal = (Goal [Comb "student_of" [Var (VarName "S"),Comb "peter" []]])
prog = parseProg "student_of(X,T):-follows(X,C),teaches(T,C).\nfollows(paul,computer_science).\nfollows(paul,expert_systems).\nfollows(maria,ai_techniques).\nteaches(adrian,expert_systems).\nteaches(peter,ai_techniques).\nteaches(peter,computer_science)."

terms (Prog t) = t
goalAs (Goal t) = t  

goalTerm = goalAs goal !! 0
firstTarget = terms prog !! 0

ruleIdent (Rule left _) = left
ruleRight (Rule _ rigths) = rigths

g = (Comb "student_of" [Var (VarName "S"),Comb "peter" []])

resses = resolutionStep goal prog

newGoal = Goal [Comb "follows" [Var (VarName "S"),Var (VarName "C")],Comb "teaches" [Comb "peter" [],Var (VarName "C")]]

newRess = resolutionStep newGoal prog