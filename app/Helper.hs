module 
App.Helper
 where

import Data.List (nub)
import Data.Set (fromList, isSubsetOf)

unique :: Eq a => [a] -> [a]
unique = nub

isSubset :: Ord a => [a] -> [a] -> Bool
isSubset a b = (fromList a) `isSubsetOf` (fromList b)

listEquals :: Ord a => [a] -> [a] -> Bool
listEquals a b = isSubset a b && isSubset b a 

sameLength :: [a] -> [b] -> Bool
sameLength a b = length a == length b