module WFreq where

import qualified Data.Map as Map
import qualified Data.Map.Strict as Map.Strict

import Data.Char (toLower)
import Data.List (sortOn)
import Data.Ord

import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

regextTrial :: [String]
regextTrial =
  let test = "  //wont won't won't , e ... \"quote\""
      r = "[a-zA-Z0-9-' ]"
  in getAllTextMatches $ test =~ r

letters :: String
letters = "abcdefghijklmnopqrstuvwxyz"

valid :: String
valid = letters ++ "' "

prepare :: String -> String
prepare = map (go . toLower)
  where go c
          | c `elem` valid = c
          | otherwise = ' '

validWord :: String -> Bool
validWord = any (`elem` letters)

validWords :: String -> [String]
validWords = filter validWord . words . filter (`elem` valid)

-- for each word, use the word as a key, add 1 to the existing value for that key
incrementCount :: (Ord k, Num a) => Map.Strict.Map k a -> k -> Map.Strict.Map k a
incrementCount m word = Map.Strict.insertWith (+) word 1 m

countWords :: [Char] -> Map.Strict.Map String Integer
countWords = foldl incrementCount Map.empty . validWords

top3 :: String -> [String]
top3 = take 3 . map fst . sortOn (Down . snd) . Map.Strict.toList . countWords . prepare

-- TODO: make this work using a reader or something so it:
--   - avoids sorting the array of unique words,
--   - avoids creating an
-- array of all the words in the input
-- top3' :: String -> [String]
-- top3' = Map.Strict.toList . countWords

-- What if:
-- get words first (`words`), then for each word, remove invalid characters. Then... count somehow? 
-- How to count, one word at a time
-- Each step:
-- - break into words
-- - for each word:
-- --- strip invalid characters
-- --- skip (Nothing?) if word is invalid (no letters)
-- --- * Count the word *


