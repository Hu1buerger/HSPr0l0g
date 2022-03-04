module App.Substitution 
 (Subst, domain, empty, single, apply, compose, restrictTo) 
where

import Data.List
import Test.QuickCheck

import App.Type
import App.Vars
import App.Helper
import App.Pretty


data Subst = Subst [(VarName, Term)]
    deriving (Eq, Show)

instance Pretty Subst where 
    pretty (Subst list) = "{" ++ (intercalate ", "  $ map (\((VarName name), term) -> name ++ " -> " ++ pretty term) list)++ "}"

instance Vars Subst where 
    extractVars (Subst su) = concatMap (\(name, term) -> name : allVars term) su

{-
instance Arbitrary Subst where 
    arbitrary = do
        listSize <- choose(2,8)
        frequency [ (1, elements [Empty])
                    , (10, suchThat (VarSub <$> arbitrary <*> arbitrary) (\e -> (not . isIdentity $ e) && (not . isKindaSelfSubstituting $ e)))
                    , (1, Composition <$> vectorOf listSize (suchThat arbitrary (isTrivialSubstitution)))
                        ]
                        -- (Composition <$> choose (2, 6) >>= \n -> (map (arbitrary) [0..n])))
-}

instance Arbitrary Subst where
    arbitrary = Subst <$> listOf arbitrary

-- keine Identitäten, das machen wir bei single und compose 
domain :: Subst -> [VarName]
--domain (Subst[])       = []
--domain (Subst(p : xs)) = fst p : domain (Subst xs)
domain (Subst s) = map fst s 

--{}
empty :: Subst 
empty = Subst []

--{X -> f(T)}
single :: VarName -> Term -> Subst
single v t = case t of 
    (Var vn) -> if v == vn then empty else Subst [(v,t)]
    (Comb c ts) -> Subst [(v,t)]

apply :: Subst -> Term -> Term
apply (Subst s) (Var vn)    = case lookup vn s of 
    -- apply {X -> A} X = A 
    (Just t) ->  t 
    -- apply {X -> A} Z = Z
    Nothing -> Var vn
apply s (Comb x ts) =  Comb x (map (\t -> apply s t ts))

--1. Schritt: Abkürzung bilden (linke Substitution auf alle rechten Seiten der rechten Substitution anwenden)
--2. Schritt: Aus der linken Substitution alle Paare hinzufügen, deren linke Seite nicht in der Domain der rechten Substitution ist
--3. Schritt: Filtere alle Abb. der Form {X-> Var X} raus
compose :: Subst -> Subst -> Subst 
compose l@(Subst s1) r@(Subst s2) = Subst (stepThree(stepOne l s2 ++ stepTwo s1 (domain r)))
    where
        stepOne :: Subst -> [(VarName, Term)] -> [(VarName, Term)]
        --stepOne _  [] = []
        --stepOne subst ((lhs,rsh):ps) = (lhs, apply subst rhs) : stepOne subst ps 

        stepOne subst ps = map (\(lhs, rhs) -> (lhs, apply subst rhs)) ps 

        stepTwo :: [(VarName, Term)] -> [VarName] -> [(VarName, Term)]
        stepTwo ps forbidden = filter (\(v,t) -> v `notElem` forbidden) ps 

        stepThree :: [(VarName, Term)] -> [(VarName, Term)]
        stepThree ps = filter help ps

        help :: (VarName, Term) -> Bool
        help (vn, Var x) = if vn /= x then True else False
        help _ = True 

restrictTo :: Subst -> [VarName] -> Subst 
--restrictTo res (Subst ((v,t):ps)) = if v `elem` res then let (Subst ps') = restrictTo res (Subst ps)
                                                        --in Subst ((v,t) : ps')
                                                   -- else restrictTo res (Subst ps)
--restrictTo res (Subst []) = Subst []

restrictTo (Subst s) res = Subst (filter (\(v,_) -> v `elem` res  ) s)
