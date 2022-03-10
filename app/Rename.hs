module 
 App.Rename (rename)
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

        (left': rights') = map (apply subst) (left : rigths)
        rule' = (Rule left' rights')
        illegals' = allillegals ++ allVars rule'
    in  fst $ deanonRules illegals' rule'

            
deanon :: [VarName] -> Term -> (Term, [VarName])
deanon illegals (Var (VarName "_")) =
    let vn' = head $ filter (`notElem` illegals) freshVars
    in ((Var vn'), vn' : illegals)
deanon illegals (Var vn) = (Var vn, illegals)
deanon illegals (Comb c ts) = 
    let 
        (terms', forbidden') = deanonList illegals ts
    in (Comb c terms', forbidden')

deanonList :: [VarName] -> [Term] -> ([Term], [VarName])
deanonList illegals [] = ([], illegals)
deanonList illegals (t : ts) = 
    let 
        (t', forbidden') = deanon illegals t
        (ts', forbidden'') = deanonList forbidden' ts
    in (t':ts', forbidden'')

deanonRules :: [VarName] -> Rule -> (Rule, [VarName])
deanonRules illegals (Rule left rigths) =
    let
        terms = left : rigths 
        (terms', forbidden') = deanonList illegals terms
        (left' : rigths') = terms'
    in (Rule left' rigths', forbidden')