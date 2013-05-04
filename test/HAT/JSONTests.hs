module HAT.JSONTests where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.HUnit

import HAT.TestUtils

import Text.JSON
import Text.JSON.String
import Text.JSON.Pretty

import HAT.Person

import Data.Functor

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

readPersonResult = readJSON personJsonObj :: Result Person
(Ok readPerson) = readPersonResult

nameResult    = name    <$> readPersonResult
surnameResult = surname <$> readPersonResult

shownPerson = showJSON person

-- Encoding and decoding

personJsonStr' = encode person

personResult = decode personJsonStr :: Result Person
(Ok person') = personResult

-- List of persons

persons = replicate 5 person
shownPersons = showJSONs persons

readPersons = readJSONs shownPersons :: Result [Person]

-- Parsing


-- Tests

tests = [
    testGroup "JSON encoding" [
      testCase "Encoding" (
        assertEqual' "{\"name\":\"Marek\",\"surname\":\"Dudek\"}" personJsonStr 
      ),
      testCase "Strict encoding" (
        assertEqual' "{\"name\":\"Marek\",\"surname\":\"Dudek\"}" personJsonStrictStr 
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
    ],
    testGroup "Encoding and decoding" [
      testCase "Encoding to string" (
        assertEqual' personJsonStr personJsonStr'
      ),
      testCase "Decoding from string" (
        assertEqual' person person'
      )
    ]
  ]
