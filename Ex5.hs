import Data.Char
import Data.List

rpcalc :: String -> String
rpcalc (x:xs) 
    | xs == [] = 0
    | x:xs==[]= 0
    | otherwise = rpcalc (( xs \\ ([a] ++ [b] ++ [nextoperator])) ++ nextval)
    where
        ints = [ x | x <- xs, isDigit x]
        symbols = [ x | x <- xs, isDigit x == False]
        nextoperator = last symbols
        a = last ints
        b = last ints 
        nextval = calc (read [a]) (read [b]) nextoperator

calc :: Integral a => Num a => a -> a -> Char -> a
calc a b operator
    | operator == '+' = a + b
    | operator == '-' = a - b
    | operator == '/' = div a b
    | operator == '*' = a * b
    | otherwise = 0


