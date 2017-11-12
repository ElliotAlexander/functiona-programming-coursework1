histogram :: Int -> [Int] -> [Int]
histogram n (x:xs)
  | x:xs == [] = []
  | x < n-1 = [1] ++ histogram n xs
  | otherwise = histogram (2*n) xs
