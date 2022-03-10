module 
 App.SLDResolution 
 where 

import Debug.Trace

import Data.Maybe

import App.Pretty
import App.Type
import App.Substitution
import App.Unification

data SLDTree = SLDTree Goal [(Subst, SLDTree)] deriving (Show, Eq)

instance Pretty SLDTree where
    pretty = unlines . drawRose
        where
            drawRose :: SLDTree -> [String]
            drawRose (SLDTree (Goal []) _) = error "something unexpected"
            drawRose (SLDTree goal []) = [pretty goal ++ " []"]
            drawRose (SLDTree goal subst) = pretty goal : aux subst

            aux :: [(Subst, SLDTree)] -> [String]
            aux [] = []
            aux [(s, tree)] = shift "+-- " "|   " ((pretty s) : drawRose tree)
            aux (t:ts)      = shift "+-- " "|   " (aux [t]) ++ aux ts

            shift :: [a] -> [a] -> [[a]] -> [[a]]
            shift first other = zipWith (++) (first : repeat other)

-- sld :: Prog -> Goal -> SLDTree
--SLDTree currentGoal $ map (\(subs, goalterms') -> (subs, sld prog (Goal goalterms')))
sld _ (Goal []) = SLDTree (Goal []) []
sld prog@(Prog programmRules) currentGoal@(Goal goals) =  
    let 
        powset = powerset goals 
        res = concat [unifyRules [] programmRules i | i <- powset]
        possibleNodes = map (\(s,g) -> (s, sld prog $ Goal g)) res
        nodes = filter (\(_,t) -> notemptySLD t) possibleNodes
    in SLDTree currentGoal nodes
    where 
        notemptySLD (SLDTree (Goal gt) _) = gt /= []
    {-unifyWide
    where 
        unifyWide :: [(Subst, [Term])]
        unifyWide = foldl (unifyOrDump) [] programmRules
        
        unifyOrDump acc rule = 
            let 
                maybeUnifyied = unifyRule [] rule goals
                subTerm = fromJust maybeUnifyied
            in if isNothing maybeUnifyied then acc
                                          else acc ++ [subTerm]
                                          -}

powerset :: [a] -> [[a]]
powerset l = reverse [take i l| i <- [1..(length l)]]

unifyRules :: [VarName] -> [Rule] -> [Term] -> [(Subst, [Term])]
unifyRules illegals rulez query = map fromJust $ filter isJust $ map (\rule -> unifyRule illegals rule query) rulez

-- verbotene Namen -> umbenannte Regel -> Terme des Goals -> Nothing not unifiyable | Just (mgu, neue RegelTerme)
unifyRule :: [VarName] -> Rule -> [Term] -> Maybe (Subst, [Term])
unifyRule _ (Rule _ _) [] = Nothing
unifyRule illegals (Rule left rights) query 
    | isJust mmgu && sigma == empty = Nothing
    | isJust mmgu = Just (sigma, map (apply sigma) $ nexts ++ rights)
    | otherwise = unifyRule illegals (Rule left rights) nexts
    where 
        current = head query
        nexts = tail query
        mmgu = unify left current
        sigma = fromJust mmgu 
