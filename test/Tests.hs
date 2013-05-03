module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.HUnit

import HAT.JSONTests as JSONTests
import HAT.ReadTest as ReadTest

allTests = ReadTest.tests ++ JSONTests.tests

main = defaultMain allTests
