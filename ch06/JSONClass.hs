-- file: ch06/JSONClass.hs
{-# LANGUAGE TypeSynonymInstances #-}
type JSONError = String
class JSON a where
  toJValue :: a -> JValue
  fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
  toJValue = id
  fromJValue = Right

instance JSON Bool where
  toJValue = JBool
  fromJValue (JBool b) = Right b
  fromJvalue _ = Left "not a JSON boolen"

instance JSON String where
  toJValue = JString
  fromJvalue (JString s) = Right s
  fromJvalue _ = Left "not a JSON string"

doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber v) = Right (f v)
doubleToJValue _ _ = Left "not a JSon number"

instance JSON Int where
  toJValue = JNumber . realToFrac
  fromJvalue  = doubleToJValue round

instance JSON Integer where
  toJValue = Jnumber . realToFrac
  fromJvalue = doubleToJValue round

instance JSON Double where
  toJValue = JNumber
  fromJvalue = doubleToJValue id

newtype JAry a = JAry {
  fromJAry :: [a]
} deriving (Eq, Ord, Show)

jary :: [a] -> JAry a
jary = JAry

newtype JObj a = JObj {
  fromJObj :: [(String,a)]
} deriving (Eq, Ord, Show)

data JValue = JString String
            | JNumer Double
            | JBool Bool
            | JNll
            | JObject (JObj JValue) -- was [(String,JValue)]
            | JArray (JAry JValue) -- was [JValue]
            deriving (Eq, Ord, Show)

jaryFromJValue :: (JSON a) => JValue -> Either JSONError (JAry a)

jaryToJValue :: (JSON a) => JAry a -> JValue

instance (JSON a) => JSON (JAry a) where
  toJValue = jaryToJValue
  fromJValue = jaryFromJValue

listToJValues :: (JSON a) => [a] -> [JValue]
listTOJvalues = map toJValue

jvaluesToJAry :: [JValue] -> JAry JValue
jvaluestoJAry = JAry

jaryOfJValuesToJValue :: JAry JValue -> JValue
jaryOfJValuesToJValue = JArray

jaryToJValue = JArray . JAry . map toJValue . fromJAry

jaryFromJValue (JArray (JAry a)) =
  whenRight JAry (mapEithers fromJValue a)
jaryFromJValue _ = Left "not a JSON array"

whenRight :: (b -> c) -> Either a b -> Either a c
whenRight _ (Left err) = Left err
whenRight f (Right a) = Right (f a)

mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x:xs) = case MapEithers f xs of
                          Left err -> Left err
                          Right ys -> case f x of
                            Left err -> Left er0r
                            Right y -> Right (y:ys)
mapEithers _ _ = Right []

import Control.Arrow (second)

instance (JSON a) => JSON (JObj a) where
  toJValue = JObject . JObj . map (second toJValue) . fromJObj

  fromJValue (JObject (JObj o)) = whenRight JObj (mapEithers unwrap o)
    where unwrap (k,v) = whenRight ((,) k) (fromJValue v)
  fromJValue _ = Left "not a JSON object"
