module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.HUnit

import Text.JSON

import HAT.Person


person = Person "Marek" "Dudek"

personJsonObj :: JSValue
personJsonObj = makeObj [
    ("name",    JSString $ toJSString $ name person),
    ("surname", JSString $ toJSString $ surname person)
  ]

personJsonStr :: String
personJsonStr = encode personJsonObj

personJsonStrictStr = encodeStrict personJsonObj

tests = [
    testGroup "JSON encoding" [
      testCase "Encoding" (
        assertEqual'  "{\"name\":\"Marek\",\"surname\":\"Dudek\"}"  personJsonStr 
      ),
      testCase "Strict encoding" (
        assertEqual'  "{\"name\":\"Marek\",\"surname\":\"Dudek\"}"  personJsonStrictStr 
      )
    ]
  ]
 
main = defaultMain tests

assertEqual' expected actual = assertEqual "" expected actual
