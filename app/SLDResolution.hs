module 
 App.SLDResolution 
 where 

import Debug.Trace

import Data.Maybe

import App.Type
import App.Substitution
import App.Unification

data SLDTree = Root Goal Node 
data Node = Node Goal Subst [Node]

--resolutionStep :: Goal -> Prog -> [(Subst, Goal)]
--resolutionStep goal (Prog rules) = map (fromJust) $ filter (isJust) $ map (resolutionStepOnGoal goal) rules
resolutionStep goal (Prog rules) = map (fromJust) $ filter (isJust) $ map (resolutionStepOnGoal goal) rules

resolutionStepOnGoal :: Goal -> Rule -> Maybe (Subst, Goal)
resolutionStepOnGoal (Goal [t]) r@(Rule left right) 
    | isNothing mmgu = Nothing
    | otherwise = Just (sigma, (Goal $ [apply sigma t]))
    where 
        mmgu = unify left t -- (traceShowId t) 
        sigma = fromJust mmgu

resolutionStepOnGoal (Goal (t:ts)) r
    | isJust step = step
    | otherwise = resolutionStepOnGoal (Goal ts) r
    where 
        step = resolutionStepOnGoal (Goal [t]) r
