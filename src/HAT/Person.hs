module HAT.Person where

import Text.JSON


data Person = Person {
  name    :: String,
  surname :: String
} deriving (Show, Read, Eq)


instance JSON Person where

  readJSON (JSObject obj) = (
      return Person {
        name    = getField "name",
        surname = getField "surname"
      }
    ) where
        getField field = 
          let (Ok value) = field `valFromObj` obj in 
          value 

  showJSON person = makeObj [
      ("name",    JSString $ toJSString $ name person),
      ("surname", JSString $ toJSString $ surname person)
    ] 
