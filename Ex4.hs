import Data.List

-- Mostly working, needs some better testing. 

data TypeOfSort = Ascending | NonDescending | Constant | NonAscending | Descending | NotSorted deriving Show
sortType :: Ord a => [a] -> TypeOfSort
sortType xs
  | xs == [] = Ascending
  | length minusdups == 1 = NonAscending
  | sort xs == xs && dup == True = Ascending
  | reverse (sort xs) == xs && dup == True = Descending
  | sort xs == xs = NonDescending
  | reverse (sort xs) == xs = NonAscending
  | otherwise = NotSorted
  where 
    minusdups = map head (group(sort xs))
    dup = length minusdups == length xs
