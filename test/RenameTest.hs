{-# LANGUAGE TemplateHaskell #-}

module Test.RenameTest where 

import Data.List(intersect)
import Test.QuickCheck

import App.Helper
import App.Type
import App.Rename
import App.Vars

prop_test1 xs r = allVars (rename xs r) `intersect` allVars r == []

prop_test2 xs r = allVars (rename xs r) `intersect` xs == []

prop_test3 xs r = (VarName "_") `notElem` allVars (rename xs r)

prop_test4 xs r = (VarName "_") `notElem` allVars r ==> sameLength (allVars $ rename xs r) (allVars r) 

prop_test5 xs r = length (allVars $ rename xs r) >= (length (allVars r))


prop_rename_anon = 1 < (length $ allVars $ rename [] (Rule (Comb "a" [Var (VarName "_"),Var (VarName "_")]) []))

-- Check all properties in this module:
return []
testAll = $quickCheckAll