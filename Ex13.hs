-- makeChange :: Int -> [Int] -> [(Integer, Integer, Integer)]
makeChange :: Int -> [Int] -> [(Int, Int)]
makeChange m denoms
    | otherwise = combined
    where
        combined = (zip [ x | x <- [1..1000]] denoms)

--changeCheck :: [Int] -> [Int] -> [Int]
--changeCheck m denoms
 --   | m == map sumPair (zip m denoms) = [0]
  --  where
  --     n = length denoms

sumPair :: (Int, Int) -> Int
sumPair (x,y) = x * y