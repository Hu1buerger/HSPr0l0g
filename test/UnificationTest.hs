{-# LANGUAGE TemplateHaskell #-}

module Test.UnificationTest where 

import Data.Maybe
import Test.QuickCheck

import App.Type 
import App.Pretty
import App.Substitution
import App.Unification

prop_test1 :: Term -> Bool
prop_test1 t = ds t t == Nothing

prop_test2 :: Term -> Term -> Property
prop_test2 t t1 = ds t t1 /= Nothing ==> t /= t1

prop_test3 :: NothingDS -> Bool
prop_test3 (NothingDS t1 t2) = unify t1 t2 /= Nothing && null (domain . fromJust $ unify t1 t2)

prop_test4 :: Term -> Term -> Property
prop_test4 t1 t2 = unify t1 t2 /= Nothing ==> (ds (apply unt1t2 t1) (apply unt1t2 t2)) == Nothing
    where unt1t2 = (fromJust $ unify t1 t2)

data NothingDS = NothingDS Term Term deriving (Show)
data JustDS = JustDS Term Term deriving (Show)

instance Arbitrary NothingDS where
    arbitrary = (uncurry NothingDS <$> (suchThat arbitrary (\(a,b) -> ds a b == Nothing)))
instance Arbitrary JustDS where 
    arbitrary = (uncurry JustDS <$> (suchThat arbitrary (\(a,b) -> ds a b /= Nothing)))


-- skript page 145 #1
prop_fromSkript1 :: Bool 
prop_fromSkript1 = (pretty $ fromJust $ unify (Comb "ehemann" [Comb "monika" [],Var (VarName "M")]) (Comb "ehemann" [Var (VarName "F"),Comb "herbert" []])) == "{M -> herbert, F -> monika}"

prop_regression1 :: Bool
prop_regression1 = (unify (Comb "student_of" [Var (VarName "S"),Comb "peter" []]) (Comb "student_of" [Var (VarName "X"),Var (VarName "T")])) == Just (compose (single (VarName "T") (Comb "peter" [])) (single (VarName "S") (Var (VarName "X"))))

-- Check all properties in this module:
return []
testAll :: IO Bool
testAll = $quickCheckAll