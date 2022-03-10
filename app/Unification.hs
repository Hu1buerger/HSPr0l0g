module 
 App.Unification 
 where

import Debug.Trace

import Data.Maybe

import App.Type
import App.Substitution
import App.Vars

ds :: Term -> Term -> Maybe (Term, Term)
ds t1@(Var vn) t2 
    | t1 /= t2 = Just (t1, t2)
    | otherwise = Nothing -- not sure. def 4.2 #2 is unspecific
ds t1 t2@(Var vn) 
    | t1 /= t2 = Just (t1, t2)
    | otherwise = Nothing
ds t1@(Comb nl t) t2@(Comb nr s)
    | nl /= nr  = Just (t1, t2)
    | length t /= length s = Just (t1, t2)
    | otherwise = tail t s
        where
            tail (tk:ts) (sk:ss)
                | tk /= sk = ds tk sk
                | otherwise = tail ts ss
            tail _ _ = Nothing

unify :: Term -> Term -> Maybe Subst
unify = fun empty
    where 
        fun :: Subst -> Term -> Term -> Maybe Subst
        fun sk t0 t1
            | dsres == Nothing = Just sk                    -- Unifikationsalgo step 2; mgu found
            | sub == Nothing = Nothing                      -- step3; sonnst fail
            | otherwise = fun (compose sk (fromJust sub)) t0 t1
            where 
                t0k = apply sk t0
                t1k = apply sk t1
                dsres = ds t0k t1k
                sub = buildSubst dsres 

        buildSubst :: Maybe (Term, Term) -> Maybe Subst
        buildSubst (Just (Var vn, term))
            | vn `notElem` allVars term = Just $ single vn term -- occurence check step 3
            | otherwise = Nothing
        -- buildSubst (Just (Var vn, Var vn2))
        --    | vn == vn2 = error "no susbstitution should be build"
        --   | otherwise = Just $ single vn (Var vn2)
        --buildSubst (Just (t1, t2)) = error ("wr-one direction?" ++ show t1 ++ " ---- " ++ show t2)
        --buildSubst (Just (term, Var vn)) = buildSubst (Just (Var vn, term))
        buildSubst _ = Nothing
