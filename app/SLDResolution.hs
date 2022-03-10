module 
 App.SLDResolution 
 where 

import Debug.Trace

import Data.Maybe
import Data.List (nub)

import App.Pretty
import App.Type
import App.Substitution
import App.Unification
import App.Rename
import App.Vars

data SLDTree = SLDTree Goal [(Subst, SLDTree)] deriving (Show, Eq)

instance Pretty SLDTree where
    pretty = unlines . drawRose
        where
            drawRose :: SLDTree -> [String]
            drawRose (SLDTree (Goal []) _) =  ["(Goal []) []"] --error "something unexpected"
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

sldHelper _ _ (Goal []) = SLDTree (Goal []) []
sldHelper illegals prog@(Prog programmRules) currentGoal@(Goal goals) =  
    let 
        progRules' = Prog $ renameRules illegals programmRules
        illegals' = nub $ illegals ++ (arrrrr prog $ Goal goals)

        powset = powerset goals 
        res = concat [unifyRules [] programmRules i | i <- powset]

        possibleNodes = map (\(s,g) -> (s, sldHelper illegals' progRules' $ Goal g)) res
        -- nodes = filter (\(_,t) -> notemptySLD t) possibleNodes
    in SLDTree currentGoal possibleNodes
    where 
        notemptySLD (SLDTree (Goal gt) _) = gt /= []

        arrrrr (Prog rulez) (Goal goalez) = nub $ (concatMap (allVars) rulez)  ++ (concatMap (allVars) goalez)

powerset :: [a] -> [[a]]
powerset l = reverse [take i l| i <- [1..(length l)]]

unifyRules :: [VarName] -> [Rule] -> [Term] -> [(Subst, [Term])]
unifyRules illegals rulez query = map fromJust $ filter isJust $ map (\rule -> unifyRule illegals rule query) rulez

-- verbotene Namen -> umbenannte Regel -> Terme des Goals -> Nothing not unifiyable | Just (mgu, neue RegelTerme)
unifyRule :: [VarName] -> Rule -> [Term] -> Maybe (Subst, [Term])
unifyRule _ (Rule _ _) [] = Nothing
unifyRule illegals (Rule left rights) query 
    -- | isJust mmgu && sigma == empty = Nothing
    | isJust mmgu = Just (sigma, map (apply sigma) $ nexts ++ rights)
    | otherwise = unifyRule illegals (Rule left rights) nexts
    where 
        current = head query
        nexts = tail query
        mmgu = unify left current
        sigma = fromJust mmgu 
