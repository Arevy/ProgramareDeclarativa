-- Based on Real World Haskell, Chapter 5 and Chapter 6

module SimpleJSON
    (
      JValue(..)
    , getString
    , getInteger
    , getBool
    , getObject
    , getArray
    , isNull
    ) where

import Data.Either


data JValue = JString String
            | JNumber Integer
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)



getInteger :: JValue -> Either String Integer
getInteger (JNumber n) = Right n
getInteger  _ = Left "Not a JNumber"


-- define accessor functions using the above example

getString :: JValue -> Either String String
getString ( JString str ) = Right str
getString _ = Left "Not a JString" 


getBool :: JValue -> Either String Bool
getBool  (JBool val ) = Right val
getBool _ = Left "Not a JBool"

getObject :: JValue -> Either String [(String,JValue)]
getObject  (JObject [(key, val)] )= Right [(key,val)]
getObject  _ = Left "Not a JObject"

getArray :: JValue -> Either String [JValue]
getArray  (JArray arr) = Right arr
getArray _ = Left "Not a JArray"

-- define isNull as a predicate that asserts that its argument is JNull 
isNull :: JValue -> Bool
isNull JNull= True
isNull _ = False 




