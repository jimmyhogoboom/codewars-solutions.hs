module Camel where

import Data.Char

toCamelCase :: String -> String
toCamelCase [] = []
toCamelCase (i:is) = i : go is False
  where go (s:ss) start
          | start = toUpper s : go ss False
          | s == '-' || s == '_' = go ss True
          | otherwise = toLower s : go ss False
        go [] _ = []
