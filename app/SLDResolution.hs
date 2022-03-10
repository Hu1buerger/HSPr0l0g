module 
 App.SLDResolution 
 where 

import Debug.Trace

import Data.Maybe

import App.Type
import App.Substitution
import App.Unification

data SLDTree = SLDTree Goal [(Subst, SLDTree)] deriving (Show)

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