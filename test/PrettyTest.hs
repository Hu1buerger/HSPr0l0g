{-# LANGUAGE TemplateHaskell #-}

module Test.PrettyTest where 

import Test.QuickCheck

import App.Type
import App.Pretty

prop_var0 = pretty (Var (VarName "A")) == "A"

prop_empty_comb1 = pretty (Comb "true" []) == "true"
prop_empty_comb2 = pretty (Comb "[]" []) == "[]"
prop_comb_3 = pretty (Comb "f" [Var (VarName "_"), Comb "true" []]) == "f(_, true)"
prop_comb_4 = pretty (Comb "f" [Comb "1" [], Comb "h" [Comb "g" [Var (VarName "B")], Comb "[]" []]]) == "f(1, h(g(B), []))"

prop_rule_5 = pretty (Rule (Comb "f" [Var (VarName "X"), Comb "true" []]) []) == "f(X, true)."
prop_rule_6 = pretty (Rule (Comb "f" [Var (VarName "X"), Comb "true" []]) [Comb "g" [Var (VarName "X")]]) == "f(X, true) :- g(X)."
prop_rule_7 = pretty (Rule (Comb "f" [Var (VarName "X"), Comb "true" []]) [Comb "g" [Var (VarName "X")], Comb "h" []]) == "f(X, true) :- g(X), h."

prop_prog_8 = pretty (Prog []) == ""
prop_prog_9 = pretty (Prog [Rule (Comb "f" [Var (VarName "X"), Comb "true" []]) []]) == "f(X, true)."
prop_prog_10 = pretty (Prog [Rule (Comb "append" [Var (VarName "[]"), Var (VarName "Ys"), Var (VarName "Ys")]) [], Rule (Comb "append" [Comb "." [Var (VarName "X"), Var (VarName "Xs")], Var (VarName "Ys"), Comb "." [Var (VarName "X"), Var (VarName "Zs")]]) [Comb "append" [Var (VarName "Xs"), Var (VarName "Ys"), Var (VarName "Zs")]]]) == "append([], Ys, Ys).\nappend(.(X, Xs), Ys, .(X, Zs)) :- append(Xs, Ys, Zs)."

prop_goal_11 = pretty (Goal []) == "?- ."
prop_goal_12 = pretty (Goal [Comb "=" [Var (VarName "X"), Comb "false" []]]) == "?- =(X, false)."
prop_goal_13 = pretty (Goal [Comb "=" [Var (VarName "X"), Comb "false" []], Comb "=" [Var (VarName "X"), Comb "true" []]]) == "?- =(X, false), =(X, true)."


-- Check all properties in this module:
return []
testAll :: IO Bool
testAll = $quickCheckAll