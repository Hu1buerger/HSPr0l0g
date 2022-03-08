{-# LANGUAGE TemplateHaskell #-}

import Data.Maybe
import Test.QuickCheck

import App.Type 
import App.Substitution
import App.Unification

prop_test1 :: Term -> Bool
prop_test1 t = ds t t == Nothing

prop_test2 :: Term -> Term -> Property
prop_test2 t t1 = ds t t1 /= Nothing ==> t /= t1

prop_test3a :: Term -> Term -> Property
prop_test3a t1 t2 = ds t1 t2 == Nothing ==> unify t1 t2 /= Nothing
prop_test3b :: Term -> Term -> Property
prop_test3b t1 t2 = ds t1 t2 == Nothing ==> null (domain . fromJust $ unify t1 t2)

prop_test4 :: Term -> Term -> Property
prop_test4 t1 t2 = unify t1 t2 /= Nothing ==> (ds (apply unt1t2 t1) (apply unt1t2 t2)) == Nothing
    where unt1t2 = (fromJust $ unify t1 t2)
    
-- Check all properties in this module:
return []
testAll = $quickCheckAll