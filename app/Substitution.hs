module App.Substitution 
-- (Subst, domain, empty, single, apply, compose, isTrivialSubstitution) 
where

import Data.List
import Test.QuickCheck

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

instance Vars Subst where 
    extractVars (VarSub var term) = var : extractVars term
    extractVars (Composition substs) = concatMap (extractVars) substs

instance Arbitrary Subst where 
    arbitrary = do
        listSize <- choose(2,8)
        frequency [ (1, elements [Empty])
                    , (10, suchThat (VarSub <$> arbitrary <*> arbitrary) (\e -> (not . isIdentity $ e) && (not . isKindaSelfSubstituting $ e)))
                    , (1, Composition <$> vectorOf listSize (suchThat arbitrary (isTrivialSubstitution)))
                        ]
                        -- (Composition <$> choose (2, 6) >>= \n -> (map (arbitrary) [0..n])))

isTrivialSubstitution :: Subst -> Bool
isTrivialSubstitution (VarSub _ _) = True
isTrivialSubstitution _ = False

isKindaSelfSubstituting :: Subst -> Bool
isKindaSelfSubstituting (VarSub name term) = elem name (allVars term)

isIdentity :: Subst -> Bool
isIdentity (VarSub vname term@(Var vname2)) = vname == vname2
isIdentity _ = False

test = do 
    item <- sample' (arbitrary :: Gen Subst)
    lines <- return $ unlines $ map (pretty) item
    putStr lines
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
apply Empty term = term
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