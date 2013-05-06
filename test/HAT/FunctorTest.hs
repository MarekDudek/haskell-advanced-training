module HAT.FunctorTest where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.HUnit

import HAT.TestUtils

import Control.Applicative
import Data.Char
import Data.Functor


-- helper functions
square = (\x -> x^2)
incremented = (\x -> x+1)
capitalize = map toUpper

-- helper declaration

type EitherInt = Either String Int

instance Eq x => Eq (ZipList x) where
  ZipList xs == ZipList xs'  =  xs == xs'

instance Show x => Show (ZipList x) where
  show (ZipList xs) = show xs


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
    testGroup "List as applicative" [
      testCase "applying over list" (
        assertEqual'  [3,4,5, 2,4,6, 1,4,9]  ( [(+2), (*2), (^2)] <*> [1,2,3] )
      ),
      testCase "applying over list" (
        assertEqual'  [3,4,5, 2,4,6, 2,4,8]  ( [(+), (*), (^)] <*> [2] <*> [1,2,3] )
      ),
      testCase "applying over list" (
        assertEqual'  [3,4,5, 2,4,6, 1,4,9]  ( [(+), (*), flip (^)] <*> [2] <*> [1,2,3] )
      ),
      testCase "applying over list" (
        assertEqual'  [1,2,3, 2,4,6, 3,6,9]  ( (*) <$> [1,2,3] <*> [1,2,3] )
      )
    ],
    testGroup "zip-list as applicative" [
      testCase "applying over zip-list" (
        assertEqual'  ( ZipList [12, 14, 16] )  ( (+) <$> ZipList [1,2,3] <*> ZipList [11, 12, 13] ) 
      ),
      testCase "applying over zip-list" (
        assertEqual'  ( ZipList [11, 12, 13] )  ( max <$> ZipList [1..] <*> ZipList [11, 12, 13] ) 
      ),
      testCase "applying over zip-list" (
        assertEqual'  ( ZipList [('d','c','r'), ('o','a','a'), ('g','t','t')] )  ( (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat" ) 
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
    testGroup "Maybe as applicative" [
      testCase "applying over maybe" (
        assertEqual'  (Just 15)  ( Just (*3) <*> (Just 5) )
      ),
      testCase "applying over maybe" (
        assertEqual'  (Just 15)  ( (*) <$> Just (3) <*> (Just 5) )
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
    ],
    testGroup "Function as applicative" [
      testCase "applying over function" (
        assertEqual'  18  ((+) <$> (+3) <*> (*2) $ 5)
      )
    ]
  ]
