module Jq.Compiler where

import Jq.Filters
import Jq.Json

type JProgram a = JSON -> Either String a

-- function to normalise index
normalizeIndex :: Int -> Int -> Int
normalizeIndex len i = if i < 0 then len + i else i

-- compiler will take parsed filters and JSON input
-- and apply parsed filters on parsed JSON input
compile :: Filter -> JProgram [JSON]
-- identity: takes its input and produces the same value as output
compile Identity inp = return [inp]
-- parenthesis: removes brackets, recursively call compile
compile (Parenthesis f) inp = compile f inp
-- field: extracts the value of a key from a JObject
compile (Field key) inp =
  case inp of
    JObject obj -> case lookup key obj of
      Just v -> return [v]
      Nothing -> Left $ show JNull -- null if no key exists
    _ -> Left $ "Expected JObject, got " ++ show inp
-- optional field: same as field but does not raise error if value of key is not found
compile (FieldOptional key) inp =
  case inp of
    JObject obj -> case lookup key obj of
      Just v -> return [v]
      Nothing -> Left $ show JNull
    _ -> Left $ show JNull -- not found so return JNull; no error raised

-- index: extracts the value at a specific index from a JArray
compile (Index i) inp =
  case inp of
    JArray arr ->
      let n = normalizeIndex (length arr) i
       in if n >= 0 && n < length arr
            then return [arr !! i]
            else Left $ show JNull -- null as no element at index i exists
    _ -> Left $ "Expected JArray, got " ++ show inp
-- optional index: same as index but does not raise error if index is out of bounds
compile (IndexOptional i) inp =
  case inp of
    JArray arr ->
      let n = normalizeIndex (length arr) i
       in if n >= 0 && n < length arr
            then return [arr !! i]
            else Left $ show JNull
    _ -> Left $ show JNull
-- slice: extracts a subarray from a JArray
compile (Slice i j) inp =
  case inp of
    JArray arr ->
      let len = length arr
          ni = normalizeIndex len i
          nj = normalizeIndex len j
       in if ni < nj && ni >= 0 && nj <= len -- slice upper bound is exclusive
            then return (take (j - i) (drop i arr))
            else return []
    _ -> Left $ "Expected JArray, got " ++ show inp
-- optional slice: same as slice but does not raise error if slice is out of bounds
compile (SliceOptional i j) inp =
  case inp of
    JArray arr ->
      let len = length arr
          ni = normalizeIndex len i
          nj = normalizeIndex len j
       in if ni < nj && ni >= 0 && nj <= len
            then return (take (j - i) (drop i arr))
            else return []
    _ -> Left $ show JNull
-- iterator: iterate through entire JArray or JObject
compile Iterator inp =
  case inp of
    JArray arr -> return arr
    JObject obj -> return (map snd obj)
    _ -> Left $ "Expected JArray or JObject, got " ++ show inp
-- optional iterator: same as iterator but does not raise error if input is not an array or object
compile IteratorOptional inp =
  case inp of
    JArray arr -> return arr
    JObject obj -> return (map snd obj)
    _ -> Left $ show JNull
-- multi-index iterator: iterate over a list of indices on a JArray,
-- supporting negative indices. For each index, return the element
compile (MultiIndexIterator idxs) inp =
  case inp of
    JArray arr ->
      let len = length arr
          results =
            [ let n = if i < 0 then len + i else i
               in if n >= 0 && n < len then arr !! n else JNull
              | i <- idxs
            ]
       in return results
    _ -> Left $ "Expected JArray for multi-index iterator, got " ++ show inp
-- optional multi-index iterator
compile (MultiIndexIteratorOptional idxs) inp =
  case inp of
    JArray arr ->
      let len = length arr
          results =
            [ let n = if i < 0 then len + i else i
               in if n >= 0 && n < len then arr !! n else JNull
              | i <- idxs
            ]
       in return results
    _ -> Left $ show JNull
-- comma: apply two filters in parallel
compile (Comma f1 f2) inp = do
  res1 <- compile f1 inp
  res2 <- compile f2 inp
  return (res1 ++ res2)

-- pipe: apply two filters in sequence
-- output of the first filter is the input of the second
compile (Pipe f g) inp =
  compile f inp >>= \intermediate ->
    combine intermediate
  where
    combine [] = return []
    combine (x : xs) =
      compile g x >>= \res ->
        combine xs >>= \rest ->
          return (res ++ rest)

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p = p
