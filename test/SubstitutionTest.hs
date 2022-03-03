{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck

import App.Type
import App.Pretty
import App.Substitution

-- pretty

prop_pretty1 = pretty empty == "{}"
prop_pretty2 = pretty (single (VarName "A") (Var (VarName "A"))) == pretty empty
prop_pretty3 = pretty (compose (single (VarName "A") (Var (VarName "A"))) (single (VarName "B") (Var (VarName "B")))) == pretty empty
prop_pretty4 = pretty (compose (single (VarName "A") (Var (VarName "B"))) (single (VarName "B") (Var (VarName "A")))) == "{A -> B}"
prop_pretty5 = pretty (compose (single (VarName "A") (Var (VarName "B"))) (single (VarName "A") (Var (VarName "C")))) == "{A -> C}"
prop_pretty6 = pretty (compose (single (VarName "D") (Var (VarName "E"))) (single (VarName "F") (Comb "f" [Var (VarName"D"), Comb "true" []])))
    == "{F -> f(E, true), D -> E}"
prop_pretty7 = pretty (compose (single (VarName "G") (Var (VarName "H"))) (single (VarName "I") (Var (VarName "J"))))
    == "{I -> J, G -> H}"


-- Check all properties in this module:
return []
testAll = $quickCheckAll