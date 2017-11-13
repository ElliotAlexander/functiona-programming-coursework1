import Data.Char


encode :: String -> [Int]
encode xs 
    | xs == [] = []
    | otherwise =  y
    where 
        y = [ fromEnum x | x <- xs]

tobinary :: Int -> [Int]
tobinary 0 = [0]
tobinary x  
    | x `mod` 2 == 1 = 1 : tobinary (x `div` 2)
    | x `mod` 2 == 0 = 0 : tobinary (x `div` 2)

output :: [Int] -> [Int]
output (x:xs)
    | (x:xs) == [] = []
    | otherwise = (reverse (tobinary x)) ++ output xs