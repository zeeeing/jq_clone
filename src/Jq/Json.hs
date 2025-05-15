module Jq.Json where

import Data.Char (isControl, ord)
import Data.List (intercalate, isSuffixOf)
import Text.Printf (printf)

data JSON
  = JNull
  | JNumber Double
  | JString String
  | JBool Bool
  | JArray [JSON]
  | JObject [(String, JSON)]

-- define show instance
instance Show JSON where
  show = go 0
    where
      go :: Int -> JSON -> String
      -- base cases
      go _ JNull = "null"
      go _ (JBool True) = "true"
      go _ (JBool False) = "false"
      go _ (JNumber n) =
        let s = show n
         in if ".0" `isSuffixOf` s then take (length s - 2) s else s -- DECLARATION: used LLM to find a function that can drop '.0' suffix
      go _ (JString s) = "\"" ++ concatMap encodeUnicode s ++ "\""
      -- arrays
      go _ (JArray []) = "[]"
      go d (JArray xs) =
        "[\n"
          ++ intercalate
            ",\n"
            [ replicate (d + 2) ' ' ++ go (d + 2) x
              | x <- xs
            ]
          ++ "\n"
          ++ replicate d ' '
          ++ "]"
      -- objects
      go _ (JObject []) = "{}"
      go d (JObject kvs) =
        "{\n"
          ++ intercalate
            ",\n"
            [ replicate (d + 2) ' '
                ++ show k
                ++ ": "
                ++ go (d + 2) v
              | (k, v) <- kvs
            ]
          ++ "\n"
          ++ replicate d ' '
          ++ "}"

-- define eq instance
instance Eq JSON where
  JNull == JNull = True
  (JNumber a) == (JNumber b) = a == b
  (JString a) == (JString b) = a == b
  (JBool a) == (JBool b) = a == b
  (JArray a) == (JArray b) = a == b
  (JObject a) == (JObject b) = a == b
  _ == _ = False

encodeUnicode :: Char -> String
encodeUnicode '\b' = "\\b"
encodeUnicode '\f' = "\\f"
encodeUnicode '\n' = "\\n"
encodeUnicode '\r' = "\\r"
encodeUnicode '\t' = "\\t"
encodeUnicode '"' = "\\\""
encodeUnicode '\\' = "\\\\"
encodeUnicode c
  | ord c >= 0x80 && ord c <= 0x9f = [c]
  | isControl c = printf "\\u%04x" (ord c)
  | otherwise = [c]

-- Smart constructors
-- These are included for test purposes and aren't meant to correspond one to one
-- with the actual constructors of the JSON datatype.
-- For the "weekly" tests to succeed fill them in so that they return
-- correct JSON values. Don't change the names or the signatures.

jsonNullSC :: JSON
jsonNullSC = JNull

jsonNumberSC :: Int -> JSON
jsonNumberSC = JNumber . fromIntegral

jsonStringSC :: String -> JSON
jsonStringSC = JString

jsonBoolSC :: Bool -> JSON
jsonBoolSC = JBool

jsonArraySC :: [JSON] -> JSON
jsonArraySC = JArray

jsonObjectSC :: [(String, JSON)] -> JSON
jsonObjectSC = JObject
