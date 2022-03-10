{-# LANGUAGE TemplateHaskell #-}

import qualified Test.PrettyTest
import qualified Test.RenameTest
import qualified Test.SubstitutionTest
import qualified Test.UnificationTest
import qualified Test.VarsTest

import Test.QuickCheck

-- Check all properties in this module:
testAll = Test.PrettyTest.testAll 
    >> Test.RenameTest.testAll 
    >> Test.SubstitutionTest.testAll
    >> Test.UnificationTest.testAll
    >> Test.VarsTest.testAll