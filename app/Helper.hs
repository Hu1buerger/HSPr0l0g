module App.Helper where

import Data.List (delete)

unique :: Eq a => [a] -> [a]
unique = fun [] 
    where
        fun acc [] = reverse acc
        fun acc (x:xs)
            | elem x acc = fun acc xs
            | otherwise = fun (x:acc) xs

unorderedEquals :: Eq a => [a] -> [a] -> Bool
unorderedEquals a b 
    | length a /= length b = False
    | otherwise = fun a b 
        where 
            fun [] [] = True
            fun (a:as) b 
                | notElem a b = False
                | otherwise = fun as (delete a b)