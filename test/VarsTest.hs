{-# LANGUAGE TemplateHaskell #-}

module Test.VarsTest where 

import Test.QuickCheck

import App.Type
import App.Vars

prop_var_1 = allVars (Var (VarName "A")) == [(VarName "A")]

prop_TermHasVar :: Term -> Property
prop_TermHasVar term = nonemptyTerm term ==> allVars term /= []

prop_UniqueTermVars :: Term -> Property
prop_UniqueTermVars term = nonemptyTerm term ==> isunique $ allVars term

prop_RuleHasVar :: Rule -> Property
prop_RuleHasVar rule = nonemptyRule rule ==> allVars rule /= []

prop_UniqueRuleVars :: Rule -> Property
prop_UniqueRuleVars rule = nonemptyRule rule ==> isunique $ allVars rule

isunique [] = True
isunique [x] = True
isunique (x:xs) = notElem x xs && isunique xs

nonemptyTerm :: Term -> Bool
nonemptyTerm (Comb _ []) = False
nonemptyTerm (Comb _ t) = any (nonemptyTerm) t
nonemptyTerm _ = True

nonemptyRule :: Rule -> Bool
nonemptyRule (Rule left rights) = any nonemptyTerm (left:rights)

-- Check all properties in this module:
return []
testAll = $quickCheckAll