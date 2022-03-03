module App.Helper where

unique :: Eq a => [a] -> [a]
unique = fun [] 
    where
        fun acc [] = reverse acc
        fun acc (x:xs)
            | elem x acc = fun acc xs
            | otherwise = fun (x:acc) xs