import Data.Char

-- I think this is working....

encode :: String -> [Int]
encode xs 
    | xs == [] = []
    | otherwise =  output y
    where 
        y = [ fromEnum x | x <- xs]


tobinary :: Int -> [Int]
tobinary x  
    | x == 0 = []
    | x `mod` 2 == 1 = 1 : tobinary (x `div` 2)
    | x `mod` 2 == 0 = 0 : tobinary (x `div` 2)

output :: [Int] -> [Int]
output (x:xs)
    | x == 0 = []
    | xs == [] = y
    | otherwise = y ++ [checkparity y] ++ output xs
    where
        y = bitwiseor (reverse(tobinary x))

bitwiseor :: [Int] -> [Int]
bitwiseor xs
    | length xs == 8 = xs
    | otherwise = addstring ++ xs
    where
        ms = 8 - length xs
        addstring = take ms (repeat 0)

checkparity :: [Int] -> Int
checkparity xs
    | (length [ x | x <- xs, x == 1]) `mod` 2 == 0 = 1
    | otherwise = 0

