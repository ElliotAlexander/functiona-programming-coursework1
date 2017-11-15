import Data.Char
import Data.List

rpcalc :: String -> Int
rpcalc xs
    | xs == [] = 0
    | otherwise = rpcalc' ints symbols
    where
        ints = reverse [digitToInt x | x <- xs, isDigit x]
        symbols = reverse [ x | x <- xs, isDigit x == False]

rpcalc' :: [Int] -> String -> Int
rpcalc' ints symbols
    | symbols == [] = 0
    | ints == [] = 0
    | otherwise = rpcalc' ((drop 2 ints) ++ [calc a b (head symbols)]) (drop 1 symbols)
    where 
        a = ints !! 0
        b = ints !! 1

calc :: Int -> Int -> Char -> Int
calc a b operator
    | operator == '+' = a + b
    | operator == '-' = a - b
    | operator == '/' = div a b
    | operator == '*' = a * b
    | otherwise = 0


