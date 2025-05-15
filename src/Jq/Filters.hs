module Jq.Filters where

data Filter
  = Identity
  | Parenthesis Filter
  | Field String -- for both identifier .field_name and generic .["field_name"]
  | FieldOptional String
  | Index Int
  | IndexOptional Int
  | Slice Int Int
  | SliceOptional Int Int
  | Iterator -- this will just be the filter representing '.[]'
  | IteratorOptional
  | MultiIndexIterator [Int] -- for the numeric case like .[1,2,3]
  | MultiIndexIteratorOptional [Int]
  | Comma Filter Filter
  | Pipe Filter Filter

instance Show Filter where
  show Identity = "."
  show (Parenthesis f) = "(" ++ show f ++ ")"
  show (Field s) = "." ++ s
  show (FieldOptional s) = "." ++ s ++ "?"
  show (Index i) = "[" ++ show i ++ "]"
  show (IndexOptional i) = "[" ++ show i ++ "]?"
  show (Slice i j) = "[" ++ show i ++ ":" ++ show j ++ "]"
  show (SliceOptional i j) = "[" ++ show i ++ ":" ++ show j ++ "]?"
  show Iterator = ".[]"
  show IteratorOptional = ".[]?"
  show (MultiIndexIterator is) = "[" ++ concatMap ((++ ",") . show) is ++ "]"
  show (MultiIndexIteratorOptional is) = "[" ++ concatMap ((++ ",") . show) is ++ "]?"
  show (Comma f1 f2) = show f1 ++ "," ++ show f2
  show (Pipe f1 f2) = show f1 ++ "|" ++ show f2

instance Eq Filter where
  Identity == Identity = True
  (Parenthesis f1) == (Parenthesis f2) = f1 == f2
  (Field s1) == (Field s2) = s1 == s2
  (FieldOptional s1) == (FieldOptional s2) = s1 == s2
  (Index i1) == (Index i2) = i1 == i2
  (IndexOptional i1) == (IndexOptional i2) = i1 == i2
  (Slice i1 j1) == (Slice i2 j2) = i1 == i2 && j1 == j2
  (SliceOptional i1 j1) == (SliceOptional i2 j2) = i1 == i2 && j1 == j2
  Iterator == Iterator = True
  IteratorOptional == IteratorOptional = True
  (MultiIndexIterator is1) == (MultiIndexIterator is2) = is1 == is2
  (MultiIndexIteratorOptional is1) == (MultiIndexIteratorOptional is2) = is1 == is2
  (Comma f1 f2) == (Comma f3 f4) = f1 == f3 && f2 == f4
  (Pipe f1 f2) == (Pipe f3 f4) = f1 == f3 && f2 == f4
  _ == _ = False

newtype Config = ConfigC {filters :: Filter} -- swapped base code 'data' with 'newtype'

-- Smart constructors
-- These are included for test purposes and
-- aren't meant to correspond one to one with actual constructors you add to Filter
-- For the tests to succeed fill them in with functions that return correct filters
-- Don't change the names or signatures, only the definitions

filterIdentitySC :: Filter
filterIdentitySC = Identity

filterStringIndexingSC :: String -> Filter
filterStringIndexingSC = Field

filterPipeSC :: Filter -> Filter -> Filter
filterPipeSC = Pipe

filterCommaSC :: Filter -> Filter -> Filter
filterCommaSC = Comma
