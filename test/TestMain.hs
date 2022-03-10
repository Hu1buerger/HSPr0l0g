{-# LANGUAGE TemplateHaskell #-}

import qualified Test.PrettyTest
import qualified Test.RenameTest
import qualified Test.SubstitutionTest
import qualified Test.UnificationTest
import qualified Test.VarsTest


-- Check all properties in this module:
testAll :: IO Bool
testAll = Test.PrettyTest.testAll 
    >> Test.RenameTest.testAll 
    >> Test.SubstitutionTest.testAll
    >> Test.UnificationTest.testAll
    >> Test.VarsTest.testAll