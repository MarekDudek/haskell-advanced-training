module HAT.Person where

import Text.JSON


data Person = Person {
  name    :: String,
  surname :: String
} deriving (Show, Read, Eq)


instance JSON Person where

  readJSON (JSObject obj) = (
      return Person {
        name    = getField obj "name",
        surname = getField obj "surname"
      }
    )

  showJSON person = makeObj [
      ("name",    JSString $ toJSString $ name person),
      ("surname", JSString $ toJSString $ surname person)
    ]

getField object field =
  let (Ok value) = field `valFromObj` object in
  value
