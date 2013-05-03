module HAT.ReadTest where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.HUnit

import HAT.TestUtils
import HAT.Person

person = Person "John" "Doe"
person' = read "Person {name = \"John\", surname = \"Doe\"}"

tests = [
    testGroup "Reading test" [
      testCase "Reading test" (
        assertEqual' person person'
      )
    ]
  ]
