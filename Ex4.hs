import Data.List

sortType :: Eq a => Ord a => Num a => [a] -> String
sortType xs
  | xs == [] = "Constant"
  | check_duplicates xs = "Constant"
  | sort xs == xs = "Ascending"
  | reverse (sort xs) == xs = "Descending"
  | otherwise = "Error!"


-- we know XS isn't empty from the above checks.
check_duplicates :: Eq a => Num a => [a] -> Bool
check_duplicates (x:xs)
  | xs == [] = True
  | x == head xs = check_duplicates xs
  | otherwise = False
