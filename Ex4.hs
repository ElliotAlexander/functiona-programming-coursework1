import Data.List

sortType :: Num a => [a] -> String
sortType a
  | strictlyIncreasing a = "Testing"


strictlyIncreasing :: Num a => [a] -> Bool
strictlyIncreasing (x:y:xs)
  | xs == [] = True
--  | x < y = (strictlyIncreasing ([y] ++ xs))
