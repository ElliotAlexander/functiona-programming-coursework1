import Data.Char

rpcalc :: String -> [Char]
rpcalc (x:xs) 
    | xs==[]=[]
    | otherwise = rpcalc xs  
    where
        ints = [ x | x <- xs, isDigit x]
        symbols = [ x | x <- xs, isDigit x == False]
        val = foldr (head symbols) (head ints) (ints !! 1) $$ ints



