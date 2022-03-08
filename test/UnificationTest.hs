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

-- Check all properties in this module:
return []
testAll = $quickCheckAll