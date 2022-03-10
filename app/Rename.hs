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

        terms = left : rigths
        newVaildHead = take (length ruleVars) freshVars
        termsAfter = snd $ applyOrAnon subst (newVaildHead, []) terms
        newLeft = head termsAfter
        newRights = tail termsAfter
    in (Rule newLeft newRights)
        where 
            
applyOrAnon :: Subst -> ([VarName], [Term]) -> [Term] -> ([VarName], [Term])
applyOrAnon subst (mintyvars, acc) terms = foldl (fun subst) (mintyvars, acc) terms
fun subst (mintyvars, acc) (Var vn)
    | vn == anonymousVarName = (nextFreshest, [(Var fv)])
    | otherwise = (freshVars, [afterApply] ++ acc)
    where 
        fv = head mintyvars
        nextFreshest = tail mintyvars
        afterApply = apply subst (Var vn)
fun subst (mintyvars, acc) (Comb cn terms) = 
    let (newMintys, combterms) = applyOrAnon subst (mintyvars, []) terms
    in (newMintys, [Comb cn combterms] ++ acc)
