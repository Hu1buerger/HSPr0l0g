module 
 App.Rename 
 where 

import Data.List 
import App.Substitution
import App.Type
import App.Vars

rename :: [VarName] -> Rule -> Rule
rename illegals rule@(Rule left rigths) =  
    let 
        ruleVars = delete anonymousVarName $ allVars rule
        allillegals = illegals ++ ruleVars
        validVars = filter (`notElem` allillegals) freshVars -- build new substitutions
        subst = foldl compose empty (zipWith (\old new -> single old (Var new)) ruleVars validVars)

        mintyVars = take (length ruleVars) validVars
        terms = left : rigths
        terms' = map (apply subst) terms

        ((left': rights'), _) = deanonList terms' mintyVars
    in (Rule left' rigths')
            
deanon :: [VarName] -> Term -> (Term, [VarName])
deanon illegals (Var vn) =
    let vn' = head $ filter (`notElem` illegals) freshVars
    in ((Var vn'), vn' : illegals)
deanon illegals (Comb c ts) = Comb c (deanonList illegals ts)

deanonList :: [VarName] -> [Term] -> ([Term], [VarName])
deanonList illegals [] = ([], illegals)
deanonList illegals (t : ts) = 
    let 
        (t', forbidden') = deanon t illegals
        (ts', forbidden'') = deanonList forbidden' ts
    in (t':ts', forbidden'')

deanonRules :: [VarName] -> Rule -> (Rule, [VarName])
deanonRules illegals (Rule left rigths) 
    let
        terms = left : rigths 
        (terms', forbidden') = deanonList illegals terms
        (left' : rigths') = terms'
    in (Rule left' rigths', forbidden')