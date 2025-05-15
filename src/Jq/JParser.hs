module Jq.JParser where

import Jq.Json
import Parsing.Parsing

parseJNull :: Parser JSON
parseJNull = do
  _ <- string "null"
  return JNull

parseJSON :: Parser JSON
parseJSON = token $ parseJNull
