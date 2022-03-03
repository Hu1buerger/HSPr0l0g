module App.Vars where

import App.Type

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

