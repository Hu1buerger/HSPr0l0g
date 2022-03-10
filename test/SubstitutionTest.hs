{-# LANGUAGE TemplateHaskell #-}

module Test.SubstitutionTest where 

import Test.QuickCheck

import Data.List (union)

import App.Type
import App.Pretty
import App.Substitution
import App.Vars
import App.Helper

-- pretty

prop_pretty1 :: Bool
prop_pretty1 = pretty empty == "{}"

prop_pretty2 :: Bool
prop_pretty2 = pretty (single (VarName "A") (Var (VarName "A"))) == pretty empty

prop_pretty3 :: Bool
prop_pretty3 = pretty (compose (single (VarName "A") (Var (VarName "A"))) (single (VarName "B") (Var (VarName "B")))) == pretty empty

prop_pretty4 :: Bool
prop_pretty4 = pretty (compose (single (VarName "A") (Var (VarName "B"))) (single (VarName "B") (Var (VarName "A")))) == "{A -> B}"

prop_pretty5 :: Bool
prop_pretty5 = pretty (compose (single (VarName "A") (Var (VarName "B"))) (single (VarName "A") (Var (VarName "C")))) == "{A -> C}"

prop_pretty6 :: Bool
prop_pretty6 = pretty (compose (single (VarName "D") (Var (VarName "E"))) (single (VarName "F") (Comb "f" [Var (VarName"D"), Comb "true" []])))
    == "{F -> f(E, true), D -> E}"

prop_pretty7 :: Bool
prop_pretty7 = pretty (compose (single (VarName "G") (Var (VarName "H"))) (single (VarName "I") (Var (VarName "J"))))
    == "{I -> J, G -> H}"

-- vars

prop_var_1 :: Bool
prop_var_1 = listEquals 
                    (allVars (compose (single (VarName "G") (Var (VarName "H"))) (single (VarName "I") (Var (VarName "J")))))
                    [VarName "H",VarName "G",VarName "J",VarName "I"]

-- no. 10 test

prop_test1 :: Term -> Bool
prop_test1 t = apply empty t == t

prop_test2 :: VarName -> Term -> Bool
prop_test2 x t = apply (single x t) (Var x) == t

prop_test3 :: Term -> Subst -> Subst -> Bool
prop_test3 t s1 s2 = apply (compose s1 s2) t == apply s1 (apply s2 t)

--prop_test3a :: Term -> Subst -> Subst -> Property
--prop_test3a t s1 s2 = isTrivialSubstitution s1 && isTrivialSubstitution s2 ==> apply (compose s1 s2) t == apply s1 (apply s2 t)

prop_test4 :: Term -> Subst -> Subst -> Bool
prop_test4 _ _ _ = domain empty == []

prop_test5 :: VarName -> Bool
prop_test5 n = domain (single n (Var n)) == []

prop_test6 :: VarName -> Term -> Property
prop_test6 x t = t /= (Var x) ==> domain (single x t) == [x]

prop_test7 :: Subst -> Subst -> Bool
prop_test7 s1 s2 = domain (compose s1 s2) `isSubset` (domain s1 `union` domain s2)

prop_test8 :: VarName -> VarName -> Property
prop_test8 x1 x2 = x1 /= x2 ==> domain (compose (single x2 (Var x1)) (single x1 (Var x2))) == [x2]

prop_test9 :: Bool
prop_test9 = allVars empty == []

prop_test10 :: VarName -> Bool
prop_test10 n = allVars (single n (Var n)) == []

-- todo allVars (single x t) seems to reverse the order
prop_test11 :: VarName -> Term -> Property
prop_test11 x t = (Var x) /= t ==> (allVars (single x t)) `listEquals` (allVars t `union` [x])

prop_test12 :: Subst -> Subst -> Bool
prop_test12 s1 s2 = allVars (compose s1 s2) `isSubset` (allVars s1 `union` allVars s2)

prop_test13 :: VarName -> VarName -> Property
prop_test13 x1 x2 = x1 /= x2 ==> allVars (compose (single x2 $ Var x1) (single x1 $ Var x2)) `listEquals` [x1, x2]

prop_test14 :: Subst -> Bool
prop_test14 s = domain s `isSubset` allVars s

prop_test15 :: [VarName] -> Bool
prop_test15 xs = (domain $ restrictTo empty xs) == []

prop_test16 :: [VarName] -> Subst -> Bool
prop_test16 xs s = (domain $ restrictTo s xs) `isSubset` xs

-- Check all properties in this module:
return []
testAll :: IO Bool
testAll = $quickCheckAll