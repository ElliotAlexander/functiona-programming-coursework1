import Data.Char


encode :: String -> [Int]
encode s 
    | s == [] = []
    | otherwise = [ fromEnum x | x <- s ]

