import Data.List

data TypeOfSort = Ascending | NonDescending | Constant | NonAscending | Descending | NotSorted deriving Show

sortType :: Ord a => [a] -> TypeOfSort
sortType xs
  | xs == [] = Ascending
  | check_identical xs = Constant
  -- Stricly increase
  | sort xs == xs && check_duplicates (sort xs) == False = Ascending
  -- Strictly decrease
  | reverse (sort xs) == xs && check_duplicates (sort xs) == False = Descending
  -- Increase or stay the same
  | sort xs == xs = NonDescending
  -- Decrease or stay the same
  | reverse (sort xs) == xs = NonAscending
  | otherwise = NotSorted


check_identical :: Ord a => [a] -> Bool
check_duplicates (x:xs)
  | xs == [] = True
  | x == head xs = check_duplicates xs
  | otherwise = False

check_duplicates :: Ord a => [a] -> Bool 
check_identical (x:xs)
  | xs == [] = true