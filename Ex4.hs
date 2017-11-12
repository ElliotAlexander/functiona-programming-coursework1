import Data.List

data TypeOfSort = Ascending | NonDescending | Constant | NonAscending | Descending | NotSorted deriving Show

sortType :: Ord a => [a] -> TypeOfSort
sortType xs
  | xs == [] = Ascending
  | length no_dup == length xs = Ascending
  where 
    no_dup = map head (group(sort xs))

    -- True it no dups
no_dup_check :: Ord a => [a] -> Bool
no_dup_check xs = length (map head (group(sort xs))) == length xs