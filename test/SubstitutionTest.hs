{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck

import App.Type
import App.Pretty
import App.Substitution
import App.Vars
import App.Helper

-- pretty

prop_pretty1 = pretty empty == "{}"
prop_pretty2 = pretty (single (VarName "A") (Var (VarName "A"))) == pretty empty
prop_pretty3 = pretty (compose (single (VarName "A") (Var (VarName "A"))) (single (VarName "B") (Var (VarName "B")))) == pretty empty
prop_pretty4 = pretty (compose (single (VarName "A") (Var (VarName "B"))) (single (VarName "B") (Var (VarName "A")))) == "{A -> B}"
prop_pretty5 = pretty (compose (single (VarName "A") (Var (VarName "B"))) (single (VarName "A") (Var (VarName "C")))) == "{A -> C}"
prop_pretty6 = pretty (compose (single (VarName "D") (Var (VarName "E"))) (single (VarName "F") (Comb "f" [Var (VarName"D"), Comb "true" []])))
    == "{F -> f(E, true), D -> E}"
prop_pretty7 = pretty (compose (single (VarName "G") (Var (VarName "H"))) (single (VarName "I") (Var (VarName "J"))))
    == "{I -> J, G -> H}"

-- vars

prop_var_1 = unorderedEquals 
                    (allVars (compose (single (VarName "G") (Var (VarName "H"))) (single (VarName "I") (Var (VarName "J")))))
                    [VarName "H",VarName "G",VarName "J",VarName "I"]

-- no. 10 test

prop_test1 :: Term -> Bool
prop_test1 t = apply empty t == t

prop_test2 :: VarName -> Term -> Bool
prop_test2 x t = apply (single x t) (Var x) == t

prop_test3 :: Term -> Subst -> Subst -> Bool
--prop_test3 t s1 s2 = isTrivialSubstitution s1 && isTrivialSubstitution s2 ==> apply (compose s1 s2) t == apply s1 (apply s2 t)
prop_test3 t s1 s2 = apply (compose s1 s2) t == apply s1 (apply s2 t)

prop_test4 :: Term -> Subst -> Subst -> Bool
--prop_test4 t s1 s2 = isTrivialSubstitution s1 && isTrivialSubstitution s2 ==> 
-- Check all properties in this module:
return []
testAll = $quickCheckAll