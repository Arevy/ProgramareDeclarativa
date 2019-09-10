{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

-- Laboratory 10
-- Based on Real World Haskell, Chapter 5 and Chapter 6

module JSONClassNew
    (
      JSON(..)
    , JValue(..)
    , JAry(..)
    , JObj(..)
    ) where
    
import Data.Either
import Data.List 
import Test.QuickCheck



-- Exercise 6
newtype JStr = JStr {
  fromJStr :: String
} deriving (Eq, Ord, Show )
    

newtype JObj a = JObj {
      fromJObj :: [(String, a)]
    } deriving (Eq, Ord, Show)

newtype JAry a  = JAry {
  fromJAry::[a]
} deriving (Eq,Ord, Show)



data JValue = JString JStr
            | JNumber Integer
            | JBool Bool
            | JNull
            | JObject (JObj JValue)   -- was [(String, JValue)]
            | JArray  (JAry JValue)             -- was [JValue]
              deriving (Eq, Ord, Show)
              
result1 :: JValue
result1 = (JObject ( JObj [("foo", JNumber 1), ("bar", JBool False)]) )

result2 :: JValue
result2 = JObject ( JObj [
  ("query", JString ( JStr "awkward squad haskell") ),
  ("estimatedCount", JNumber 3920),
  ("moreResults", JBool True),
  ("results", JArray ( JAry [
     JObject( JObj [
      ("title", (JString (JStr "Simon Peyton Jones: papers") ) ) ,
      ("snippet", (JString (JStr "Tackling the awkward ...") ) ),
      ("url", (JString (JStr "http://.../marktoberdorf/") ) )
     ] ) ] ) )
  ] )


  
renderJValue :: JValue -> String
renderJValue (JString (JStr str ) ) = str
renderJValue (JBool val ) = show val
renderJValue (JNumber nr ) = show nr
renderJValue JNull = "null"
renderJValue (JArray (JAry list ) ) = "[\n" ++ (intercalate ",\n" ( map renderJValue list ) ) ++ "\n]"
renderJValue (JObject (JObj list) ) = "{\n" ++ ( intercalate ",\n" ( map (\(key,val) -> key ++ ":" ++ (renderJValue val) ) list) ) ++ "\n}"           


-- Exercise 7

type JSONError = String

class JSON a where
    toJValue :: a -> JValue    
    fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
  toJValue x = x
  fromJValue x = Right x

instance JSON Bool where
  toJValue x = JBool x
  fromJValue (JBool x) = Right x
  fromJValue _ = Left "Not a JBool value given" 

instance JSON Integer where
  toJValue x = JNumber x
  fromJValue (JNumber x) = Right x
  fromJValue _ = Left "Not a JNumber value given"

instance JSON String where
  toJValue x = JString (JStr x)
  fromJValue (JString (JStr x) ) = Right x
  fromJValue _ = Left "Not a JString value given"

instance (JSON a) => JSON (JAry a) where
  toJValue (JAry list) = JArray ( JAry [ toJValue x| x<-list] )
  -- fromJValue( JArray (JAry jvlist ) ) = Right ( JAry [ fromJValue x | x <- jvlist ] )
  -- fromJValue _ = Left "Not a JArray value given"


instance (JSON a) => JSON (JObj a) where
  toJValue (JObj list) = JObject ( JObj [ (key, toJValue val ) | (key, val) <- list ] )
  -- fromJValue ( JObject ( JObj list)) =  Right ( JObj [ (key, fromJValue val ) | (key, val) <- list ] )
  -- fromJValue _ = Left "Not a JObject value given"
   
