module HAT.FunctorTest where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.HUnit

import HAT.TestUtils

import Data.Char
import Data.Functor

-- helper functions
square = (\x -> x^2)
incremented = (\x -> x+1)
capitalize = map toUpper
-- helper declaration
type EitherInt = Either String Int

putCapStrLn = do
    fmap capitalize (putStrLn "costam")
    

tests = [
    testGroup "List as functor" [
      testCase "f-mapping over list" (
        assertEqual'  [1,4,9,16,25]  ( fmap square [1..5] )
      ),
      testCase "f-mapping over list with infix synonym" (
        assertEqual'  [1,4,9,16,25]  ( square <$> [1..5] )
      ),
      testCase "replacing all locations in map" (
        assertEqual'  [10, 10, 10, 10, 10]  ( 10 <$ [1..5] )
      )
    ],
    testGroup "Maybe as functor" [
      testCase "f-mapping over maybe" (
        assertEqual'  (Just 25)  ( fmap square (Just 5) )
      ),
      testCase "f-mapping over maybe with infix synonym" (
        assertEqual'  (Just 25)  ( square <$> (Just 5) )
      ),
      testCase "replacing location in Maybe" (
        assertEqual'  (Just 10)  ( 10 <$ (Just 5) )
      )
    ],
    testGroup "Either as functor" [
      testCase "f-mapping over Either" (
        assertEqual'  (Right 25 :: EitherInt)  ( fmap square (Right 5 :: EitherInt) )
      ),
      testCase "f-mapping over Either with infix synonym" (
        assertEqual'  (Right 25 :: EitherInt)  ( square <$> (Right 5 :: EitherInt) )
      ),
      testCase "replacing location in Either" (
        assertEqual'  (Right 10 :: EitherInt)  ( 10 <$ (Right 5 :: EitherInt) )
      )
    ],
    testGroup "Either as functor" [
      testCase "f-mapping over function" (
        assertEqual'  9  ((fmap square incremented) 2)
      ),
      testCase "f-mapping over function with infix synonym" (
        assertEqual'  9  ((square <$> incremented) 2)
      ),
      testCase "replacing location in function" (
        assertEqual'  3  ((3 <$ square) 7)
      ),
      testCase "replacing location in function, ii" (
        assertEqual'  9  ((square <$ incremented) 7 3) -- What is that?
      )
    ]
  ]
