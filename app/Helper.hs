module 
App.Helper
 where

import Data.List (delete, nub)
import Data.Set (fromList, isSubsetOf)

unique :: Eq a => [a] -> [a]
unique = nub
{-unique = fun [] 
    where
        fun acc [] = reverse acc
        fun acc (x:xs)
            | elem x acc = fun acc xs
            | otherwise = fun (x:acc) xs
-}

unorderedEquals :: Eq a => [a] -> [a] -> Bool
unorderedEquals a b 
    | length a /= length b = False
    | otherwise = fun a b 
        where 
            fun [] [] = True
            fun (a:as) b 
                | notElem a b = False
                | otherwise = fun as (delete a b)

isSubset :: Ord a => [a] -> [a] -> Bool
isSubset a b = (fromList a) `isSubsetOf` (fromList b)

listEquals :: Ord a => [a] -> [a] -> Bool
listEquals a b = isSubset a b && isSubset b a 

sameLength :: [a] -> [b] -> Bool
sameLength a b = length a == length b