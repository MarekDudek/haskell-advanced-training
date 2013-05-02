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

-- Encoding and decoding

personJsonStr :: String
personJsonStr = encode personJsonObj

personJsonStrictStr :: String
personJsonStrictStr = encodeStrict personJsonObj

(Ok personJsonObj')  = decode personJsonStr :: Result JSValue
(Ok personJsonObj'') = decode personJsonStrictStr :: Result JSValue

(Ok personJsonObj''')  = decodeStrict personJsonStr :: Result JSValue
(Ok personJsonObj'''') = decodeStrict personJsonStrictStr :: Result JSValue

-- Wrapper types

jsObj :: JSObject Integer
jsObj = toJSObject [("first", 1), ("second", 2)]

jsStr = encode jsObj

-- Serialization to and from JSON values

(Ok readPerson) = readJSON personJsonObj :: Result Person

shownPerson = showJSON person

-- Tests

tests = [
    testGroup "JSON encoding" [
      testCase "Encoding" (
        assertEqual'  "{\"name\":\"Marek\",\"surname\":\"Dudek\"}"  personJsonStr 
      ),
      testCase "Strict encoding" (
        assertEqual'  "{\"name\":\"Marek\",\"surname\":\"Dudek\"}"  personJsonStrictStr 
      )
    ],
    testGroup "JSON encoding" [
      testCase "Decoding" (
        assertEqual' personJsonObj personJsonObj'
      ),
      testCase "Decoding strict result" (
        assertEqual' personJsonObj personJsonObj''
      ),
      testCase "Strict decoding" (
        assertEqual' personJsonObj personJsonObj'''
      ),
      testCase "Strict decoding strict result" (
        assertEqual' personJsonObj personJsonObj''''
      )
    ],
    testGroup "Serialization from and to JSON object" [
      testCase "Serialization from JSON" (
        assertEqual' person readPerson
      ),
      testCase "Serialization to JSON" (
        assertEqual' personJsonObj shownPerson
      )
    ]
  ]
 
main = defaultMain tests

assertEqual' expected actual = assertEqual "" expected actual
