module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.HUnit

tests = [
    testGroup "Group name" [
      testCase "Case name" (
        assertBool "" True
      )
    ]
  ]
 
main = defaultMain tests

