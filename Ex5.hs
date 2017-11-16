import Data.Char
import Data.List

-- Pretty sure this works to some degree, rounding is weird when dividing

rpcalc :: String -> Int
rpcalc xs
    | otherwise = rpcalc' ints symbols 
    where
        ints = [digitToInt x | x <- xs, isDigit x]
        symbols = [ x | x <- xs, isDigit x == False]

rpcalc' :: [Int] -> [Char] -> Int
rpcalc' ints symbols 
    | length ints == 1 = head ints
    | otherwise = rpcalc' ([result]++(drop 2 ints)) (drop 1 symbols)
    where
        result = calc (ints !! 0) (ints !! 1) (head symbols)

calc :: Int -> Int -> Char -> Int
calc a b operator
    | operator == '+' = a + b
    | operator == '-' = a - b
    | operator == '/' = div a b
    | operator == '*' = a * b
    | otherwise = 0


