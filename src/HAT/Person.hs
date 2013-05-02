module HAT.Person where

import Text.JSON


data Person = Person {
  name    :: String,
  surname :: String
} deriving (Eq, Show)


instance JSON Person where
  readJSON (JSObject obj) = (
      Ok Person {
        name    = getField "name",
        surname = getField "surname"
      }
    ) where
        getField field = 
          let (Ok value) = field `valFromObj` obj in 
          value 

  showJSON = undefined
  readJSONs = undefined
  showJSONs = undefined
