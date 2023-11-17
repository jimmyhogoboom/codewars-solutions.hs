module Main where

import Text.ParserCombinators.Parsec (oneOf, parse, Parser)

data Direction = North | South | East | West

parseDirection :: Parser Direction
parseDirection = do
  d <- oneOf "nsew"
  return $ case d of
    'n' -> North
    's' -> South
    'e' -> East
    'w' -> West

readDirection = parse parseDirection "Failed to parse"

updateCoords (x, y) d = case d of
  North -> (x, y + 1)
  South -> (x, y - 1)
  East -> (x + 1, y)
  West -> (x - 1, y)

calculateCoords :: [Direction] -> (Int, Int)
calculateCoords = foldl updateCoords (0,0)

walkToCoord walk = calculateCoords <$> mapM (readDirection . (:[])) walk

isValidWalk :: [Char] -> Bool
isValidWalk walk = validLength && returnsHome
  where returnsHome = walkToCoord walk == Right (0, 0)
        validLength = case walk of
          [] -> False
          _ -> length (take 11 walk) == 10

-- test = ['n', 'e', 's', 'w', 'w', 'w', 'n', 's', 'e', 'e']
test = "neswwwnsee"

-- main = print $ calculateCoords <$> (sequence $ map readDirection test_s)
-- main = print $ calculateCoords <$> mapM (readDirection . (:[])) test
main = print $ isValidWalk test

-- main = do
--   print "Gimme a direction: "
--   d <- getLine
--   print $ updateCoords (0,0) <$> readDirection d
--   print "Done."
-- main = print ""
