module Trib where

import Data.Sequence (fromList, viewr)

lastN n = reverse . take n . reverse

-- i > n -> tibs
-- tibs[i-2] + tibs[i-1] + tibs[i]

-- tribonacci (a, b, c) n =
--   tribs [a, b, c] 4
--   where
--     tribs ts i
--       | i > n = ts
--       | otherwise = tribs (reverse (sum (take 3 $ reverse ts) : reverse ts)) (i + 1)

tribonacciBad :: (Num a, Eq a) => (a, a, a) -> Int -> [a]
tribonacciBad (a, b, c) n
  | n > 2 = case (a, b, c) of
      (0, 0, 0) -> replicate n 0
      _ -> tribs [a, b, c] 4
  | n > 0 = take n [a,b,c]
  | otherwise = []
  where
    tribs ts i
      | i > n = ts
      | otherwise = tribs (reverse (sum (take 3 $ reverse ts) : reverse ts)) (i + 1)

-- tribonacci (a, b, c) n =
--   tribs [a, b, c] 0
--   where
--     tribs ts i
--       | i > n = ts
--       | otherwise = sum (tribs ts (i-1)) : ts

-- foldl (/) 1 [1,2,3] = 1 / 1 / 2 / 3 = 0.1666...
-- foldr (/) 1 [1,2,3] = 3 / 2 / 1 / 1 = 1.5
-- foldr -> 1 / 1, [2,3] -> 2 / 1 / 1, [3] -> 3 / 2 / 1 / 1, []

-- trib =
--   1 : 1 : 1 : map (+) (tail trib)

-- given a list, sum the final 3 elements, apply to end of list
-- result is list + (sum of last 3 elements of list)

-- Try adding list + (tail list) + (tail $ tail list)

-- trib 3 = (a + b + c)
-- trib 4 = (b + c + (a + b + c))
-- trib 5 = (c + (a + b + c) + (b + c + (a + b + c)))

-- so trib 6 = trib 5 + trib 4 + trib 3

-- [
--  a,
--  b,
--  c,
--  (a + b + c),
--  (b + c + (a + b + c)),
--  (c + (a + b + c) + (b + c + (a + b + c))),
--  ((a + b + c) + (b + c + (a + b + c)) + (c + (a + b + c) + (b + c + (a + b + c)))
-- ]

-- And after all that, the simple solution is:
tribonacci :: Num a => (a, a, a) -> Int -> [a]
tribonacci _ 0 = []
tribonacci (a, b, c) n = a : tribonacci (b, c, a+b+c) (n-1)

-- I was close to pinning down the shorter logic!
-- The tricks that would have made this possible for me to discover:
-- 1. using n and subtracting from it to count
-- 2. Just cons the first value to the next step!
