module App.Substitution 
--(Subst) 
where

import Data.List

import App.Type
import App.Vars
import App.Helper
import App.Pretty


data Subst = VarSub VarName Term | Composition [Subst] | Empty
    deriving (Eq, Show)

instance Pretty Subst where 
    pretty (Empty) = "{}"
    pretty (VarSub vname term) = "{" ++ show vname ++ " -> " ++ pretty term ++ "}"
    pretty (Composition cs) = "{" ++ (intercalate ", " . map pretty $ cs)
{-
The domain dom(σ) of a substitution σ is commonly defined as the set of variables actually replaced, i.e. dom(σ) = { x ∈ V | xσ ≠ x }. 
As per "On the definition of substitution, replacement and allied notions in a abstract formal system", 1952, Haskell B. Curry  

Da single kein X -> X zulässt, ist die Domain die linke seite
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
    | otherwise = fun x y
        where 
            fun x y = 
                let 
                    afterApply = applySub x y
                    notIncludedY = filter (\(VarSub name _) -> substByName name afterApply == []) (toList y)
                    substitutedLeft = filter (\x -> x /= Empty) afterApply
                    res = substitutedLeft ++ notIncludedY
                in if length res == 1 
                    then head res
                    else  (Composition res)


            substByName :: VarName -> [Subst] -> [Subst]
            substByName name = filter (\(VarSub n0 _) -> name == n0)

            -- should only return VarSubs
            toList :: Subst -> [Subst]
            toList (Composition ts) = concatMap toList ts
            toList term = [term]

            applySub :: Subst -> Subst -> [Subst]
            applySub a b = concatMap (\e -> applySub e b) (toList a)