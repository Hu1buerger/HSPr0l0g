{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck

import App.Type
import App.Pretty

prop_pretty_a = pretty (Var (VarName "A")) == "A"

prop_empty_comb1 = pretty (Comb "true" []) == "true"
prop_empty_comb2 = pretty (Comb "[]" []) == "[]"
prop_comb_3 = pretty (Comb "f" [Var (VarName "_"), Comb "true" []]) == "f(_, true)"
prop_comb_4 = pretty (Comb "f" [Comb "1" [], Comb "h" [Comb "g" [Var (VarName "B")], Comb "[]" []]]) == "f(1, h(g(B), []))"

prop_rule_5 = pretty (Rule (Comb "f" [Var (VarName "X"), Comb "true" []]) []) == "f(X, true)."

-- Check all properties in this module:
return []
testAll = $quickCheckAll