module App.SubstType where

import Test.QuickCheck

import Data.List 

import App.Pretty
import App.Helper
import App.Type
import App.Vars

data Subst = Subst [(VarName, Term)]
    deriving (Eq, Show)

instance Arbitrary Subst where
    arbitrary = do
        names <- listOf (arbitrary :: Gen VarName)
        terms <- listOf (arbitrary :: Gen Term) 
        let subs = zip (unique names) (unique terms)
        return (Subst $ subs)

instance Pretty Subst where 
    pretty (Subst list) = "{" ++ (intercalate ", "  $ map (\((VarName name), term) -> name ++ " -> " ++ pretty term) list)++ "}"

instance Vars Subst where 
    extractVars (Subst su) = concatMap (\(name, term) -> allVars term ++ [name]) su