module Pop where

yearChange p0 percent aug =
  p0 + floor (fromIntegral p0 * (percent / 100)) + aug

{-|
  iterate will apply the function to the argument, then to that result, etc.
  Basically, if you need to do some calculation iteratively, building on the previous value, you use iterate.
  iterate gives you a list, so we can get all the values until they exceed our condition with takeWhile.
  Then we can get the length of the resulting list to discover how many iterations we performed.
-}
nbYear :: Int -> Double -> Int -> Int -> Int
nbYear p0 percent aug p =
   length $ takeWhile(< p) $ iterate nextYear p0
   where nextYear x = x + floor (fromIntegral x * (percent / 100)) + aug

-- nbYear :: Int -> Double -> Int -> Int -> Int
-- nbYear p0 percent aug p =
--   let
--     n t p0 percent aug p =
--       if p0 >= p then t
--       else n (t + 1) nextPop percent aug p 
--         where nextPop = yearChange p0 percent aug
--   in n 0 p0 percent aug p 

-- nbYear :: Int -> Double -> Int -> Int -> Int
-- nbYear p0 percent aug p =
--   n 0 p0 percent aug p 
--   where
--     n t p0 percent aug p =
--       if p0 >= p then t
--       else n (t + 1) nextPop percent aug p 
--         where nextPop = yearChange p0 percent aug
    

-- yearChange :: Int -> Double -> Int -> Double
-- yearChange p0 percent aug =
--   fromIntegral p0 + (fromIntegral p0 * (percent / 100)) + fromIntegral aug

-- nbYear :: Int -> Double -> Int -> Int -> Int
-- nbYear p0 percent aug p =
--   let
--     n t p0 percent aug p =
--       if p0 >= p then t
--       else n (t + 1) nextPop percent aug p 
--         where nextPop = round $ yearChange p0 percent aug
--   in n 0 p0 percent aug p 
