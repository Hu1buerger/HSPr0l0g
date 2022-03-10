module 
 App.Rename 
 where 

import App.Substitution
import App.Type
import App.Vars

rename :: [VarName] -> Rule -> Rule
rename illegals rule@(Rule left rigths) =  
    let 
        ruleVars = allVars rule
        allillegals = anonymousVarName : illegals ++ ruleVars
        validVars = filter (`notElem` allillegals) freshVars -- build new substitutions
        subst = foldl compose empty (zipWith (\old new -> single old (Var new)) ruleVars validVars)
        newLeft = apply subst left
        newRights = map (apply subst) rigths
    in (Rule newLeft newRights)