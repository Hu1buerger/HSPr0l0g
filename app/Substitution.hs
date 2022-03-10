module App.Substitution 
 (Subst, domain, empty, single, apply, compose, restrictTo) 
where

import App.SubstType

import App.Type
import App.Helper

-- keine Identitäten, das machen wir bei single und compose 
domain :: Subst -> [VarName]
domain (Subst s) = unique $ map fst s 

--{}
empty :: Subst 
empty = Subst []

--{X -> f(T)}
single :: VarName -> Term -> Subst
single v t = case t of 
    (Var vn) -> if v == vn then empty else Subst [(v,t)]
    (Comb _ _) -> Subst [(v,t)]

apply :: Subst -> Term -> Term
apply (Subst s) (Var vn)    = case lookup vn s of 
    -- apply {X -> A} X = A 
    (Just t) ->  t 
    -- apply {X -> A} Z = Z
    Nothing -> Var vn
apply s (Comb x ts) =  Comb x (map (apply s) ts)

--1. Schritt: Abkürzung bilden (linke Substitution auf alle rechten Seiten der rechten Substitution anwenden)
--2. Schritt: Aus der linken Substitution alle Paare hinzufügen, deren linke Seite nicht in der Domain der rechten Substitution ist
--3. Schritt: Filtere alle Abb. der Form {X-> Var X} raus
compose :: Subst -> Subst -> Subst 
compose l@(Subst s1) r@(Subst s2) = Subst (stepThree(stepOne l s2 ++ stepTwo s1 (domain r)))
    where
        stepOne :: Subst -> [(VarName, Term)] -> [(VarName, Term)]
        stepOne subst ps = map (\(lhs, rhs) -> (lhs, apply subst rhs)) ps 

        stepTwo :: [(VarName, Term)] -> [VarName] -> [(VarName, Term)]
        stepTwo ps forbidden = filter (\(v,_) -> v `notElem` forbidden) ps 

        stepThree :: [(VarName, Term)] -> [(VarName, Term)]
        stepThree ps = filter help ps

        help :: (VarName, Term) -> Bool
        help (vn, Var x) = vn /= x
        help _ = True 

restrictTo :: Subst -> [VarName] -> Subst
restrictTo (Subst s) res = Subst (filter (\(v,_) -> v `elem` res) s)