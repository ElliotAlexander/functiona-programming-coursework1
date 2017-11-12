import Data.List

-- COMPLETE

histogram :: Int -> [Int] -> [Int]
histogram n xs
  | xs == [] = []
  | otherwise = [length y] ++ histogram (2*n) (xs \\ y)
  where y =  [x | x <- xs, x <= n-1]
