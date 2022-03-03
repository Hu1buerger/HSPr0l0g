module App.Substitution (Subst, domain, empty, single, apply, compose) where

import Data.List

import App.Type
import App.Vars
import App.Helper
import App.Pretty


data Subst = VarSub VarName Term | Composition [Subst] | Empty
    deriving (Eq, Show)

instance Pretty Subst where 
    pretty (Empty) = "{}"
    pretty (VarSub (VarName vname) term) = "{" ++ vname ++ " -> " ++ pretty term ++ "}"
    pretty (Composition cs) = "{" ++ (intercalate ", " . map (\(VarSub (VarName vname) term) -> vname ++ " -> " ++ pretty term) $ cs) ++ "}"
{-
The domain dom(σ) of a substitution σ is commonly defined as the set of variables actually replaced, i.e. dom(σ) = { x ∈ V | xσ ≠ x }. 
As per "On the definition of substitution, replacement and allied notions in a abstract formal system", 1952, Haskell B. Curry  

Da single kein X -> X zulässt, ist die Domain die linke seite

Hypothese: Eine Subst die nicht auf sich selber abbildet sei X -> H(X)
-}
domain :: Subst -> [VarName]
domain (VarSub varname term) = [varname]
domain (Composition cs) = unique $ concatMap domain cs


empty :: Subst
empty = Empty

{-
allowing the Substituion X -> h(X)
-}
single :: VarName -> Term -> Subst
single vname term@(Var vname2) 
    | vname == vname2 = empty
    | otherwise = VarSub vname term
single vname term = VarSub vname term

apply :: Subst -> Term -> Term
apply subst@(VarSub substvarname substterm) term = 
    case term of
        (Var vname) ->  if vname /= substvarname then term
                        else substterm
        (Comb cname terms) -> Comb cname $ map (apply subst) terms

apply (Composition s) term = applyAll term s
    where 
        applyAll term [] = term
        applyAll term (x:xs) = applyAll (apply x term) xs

{-
See page 6 https://ai.ia.agh.edu.pl/_media/pl:dydaktyka:pp:prolog-substitutions-unification.pdf
-}
compose :: Subst -> Subst -> Subst
compose x y 
    | x == Empty || y == Empty = if x == Empty then y else x
    | otherwise =  
            let res = filter (\x -> x /= Empty) $ applyRight x y ++ notInDom x y
                    where 
                        notInDom l r = filter (\(VarSub n0 t0) -> notElem n0 (domain r)) (toList l)
                        applyRight l r = map (\(VarSub n t) -> (single n (apply l t))) (toList r)

                        -- should only return VarSubs
                        toList :: Subst -> [Subst]
                        toList (Composition ts) = concatMap toList ts
                        toList term = [term]
            in if length res == 1
                then head res
                else (Composition res)