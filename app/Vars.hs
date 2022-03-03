module App.Vars where

import Data.Char (ord, chr)

import App.Type

az :: [Char]
--az = map (chr) $ iterate (\x -> if x < ord 'Z' then x + 1 else ord 'A') (ord 'A')
az = map chr $ [(ord 'A')..(ord 'Z')]

freshVars = map fmtTpl [(c, i) | i <- [-1..], c <- az]
    where 
        fmtTpl :: (Char, Integer) -> String
        fmtTpl (c, n)
            | n == -1 = [c]
            | otherwise = [c] ++ show n


--freshVars :: [VarName]
--freshVars = ["A"..."Z"]
class Vars a where 
    allVars :: a -> [VarName]
    extractVars :: a -> [VarName]

    allVars = uniqueVars . extractVars
        where 
            uniqueVars :: [VarName] -> [VarName]
            uniqueVars = fun []
                where 
                    fun :: [VarName] -> [VarName] -> [VarName]
                    fun acc [] = acc
                    fun acc (x:xs)
                        | elem x acc = fun acc xs
                        | otherwise = fun (x:acc) xs

instance Vars Term where 
    extractVars (Var varname) = [varname]
    extractVars (Comb _ terms) = concatMap extractVars terms

instance Vars Rule where 
    extractVars (Rule left rights) = extractVars left ++ concatMap extractVars rights

instance Vars Prog where
    extractVars (Prog rules) = concatMap allVars rules

instance Vars Goal where 
    extractVars (Goal terms) = concatMap allVars terms

