module 
 App.SLDResolution 
 where 

import Debug.Trace

import Data.Maybe

import App.Type
import App.Substitution
import App.Unification

data SLDTree = SLDTree Goal [(Subst, SLDTree)] deriving (Show)

-- sld :: Prog -> Goal -> SLDTree
--SLDTree currentGoal $ map (\(subs, goalterms') -> (subs, sld prog (Goal goalterms')))
sld prog@(Prog programmRules) currentGoal@(Goal goals) =  unifyWide
    where 
        unifyWide :: [(Subst, [Term])]
        unifyWide = foldl (unifyOrDump) [] programmRules
        
        unifyOrDump acc rule = 
            let 
                maybeUnifyied = unifyRule [] rule goals
                subTerm = fromJust maybeUnifyied
            in if isNothing maybeUnifyied then acc
                                          else acc ++ [subTerm]

powerset :: [a] -> [[a]]
powerset l = reverse [take i l| i <- [1..(length l)]]

unifyRules :: [VarName] -> [Rule] -> [Term] -> [(Subst, [Term])]
unifyRules illegals rulez query = map fromJust $ filter isJust $ map (\rule -> unifyRule illegals rule query) rulez

-- verbotene Namen -> umbenannte Regel -> Terme des Goals -> Nothing not unifiyable | Just (mgu, neue RegelTerme)
unifyRule :: [VarName] -> Rule -> [Term] -> Maybe (Subst, [Term])
unifyRule illegals (Rule left rights) query 
    | isJust mmgu = Just (sigma, map (apply sigma) $ nexts ++ rights)
    | otherwise = Nothing
    where 
        current = head query
        nexts = tail query
        mmgu = unify left current
        sigma = fromJust mmgu 
