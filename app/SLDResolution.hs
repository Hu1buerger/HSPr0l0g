module 
 App.SLDResolution 
 where 

import Data.Maybe
import Data.List (nub)

import App.Pretty
import App.Type
import App.Substitution
import App.Unification
import App.Rename
import App.Vars

data SLDTree = SLDTree Goal [(Subst, SLDTree)] deriving (Show, Eq)

type Strategy = SLDTree -> [Subst]

instance Pretty SLDTree where
    pretty = unlines . drawRose
        where
            drawRose :: SLDTree -> [String]
            drawRose (SLDTree (Goal []) _) =  ["(Goal []) []"]
            drawRose (SLDTree goal []) = [pretty goal ++ " []"]
            drawRose (SLDTree goal subst) = pretty goal : aux subst

            aux :: [(Subst, SLDTree)] -> [String]
            aux [] = []
            aux [(s, tree)] = shift "+-- " "|   " ((pretty s) : drawRose tree)
            aux (t:ts)      = shift "+-- " "|   " (aux [t]) ++ aux ts

            shift :: [a] -> [a] -> [[a]] -> [[a]]
            shift first other = zipWith (++) (first : repeat other)

sld :: Prog -> Goal -> SLDTree
sld prog goal = sldHelper (allVars prog) prog goal 

sldHelper :: [VarName] -> Prog -> Goal -> SLDTree
sldHelper _ _ (Goal []) = SLDTree (Goal []) []
sldHelper illegals prog@(Prog programmRules) currentGoal@(Goal goals) =  
    let 
        
        progRules' = renameRules illegals programmRules
        illegals' = nub $ illegals ++ (arrrrr prog $ Goal goals)

        maybeNodes = map (unifyRule goals) progRules'
        nodes = map fromJust $ filter isJust maybeNodes

        subTrees = map (\(s,g) -> (s, sldHelper illegals' (Prog progRules') $ Goal g)) nodes

    in SLDTree currentGoal subTrees
    where 
        arrrrr (Prog rulez) (Goal goalez) = nub $ (concatMap (allVars) rulez)  ++ (concatMap (allVars) goalez)

-- umbenannte Regel -> Terme des Goals -> Nothing not unifiyable | Just (mgu, neue RegelTerme)
unifyRule :: [Term] -> Rule -> Maybe (Subst, [Term])
unifyRule []     _                  = Nothing
unifyRule (t:ts) (Rule lhs rhs) 
    | isJust mmgu = Just (sigma, map (apply sigma) (rhs ++ ts))
    | otherwise = Nothing
    where 
        mmgu = unify lhs t
        sigma = fromJust mmgu

solveWith :: Prog -> Goal -> Strategy -> [Subst]
solveWith p g s = map (flip restrictTo (allVars g)) (s (sld p g))

dfs :: SLDTree -> [Subst]
dfs t' = fun (empty, t') where
 fun :: (Subst, SLDTree) -> [Subst]
 -- Heureka, Erfolg!
 fun (s, SLDTree (Goal []) _ ) = [s]
 -- Misserfolg
 fun (_, SLDTree (Goal _) []) = []
 -- Bilde rekursiv Kante von Root zu den Unterknoten.
 fun (s, SLDTree (Goal  _) cs) = concatMap (\(sub, t) -> fun (compose sub s, t)) cs

bfs :: SLDTree -> [Subst]
bfs t' = fun [(empty, t')]
 where
  fun :: [(Subst, SLDTree)] -> [Subst]
  fun [] = []
  fun ((s, SLDTree (Goal [])  _):q) = s : fun q
  fun ((_, SLDTree (Goal  _) []):q) = fun q
  fun ((s, SLDTree (Goal  _) cs):q) = fun (q ++ map (\(sub, t) -> (compose sub s, t)) cs)