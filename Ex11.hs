import Data.Char
import Data.List

-- WORKING

decode :: [Int] -> String
decode xs
    | xs == [] = []
    | otherwise = [chr (toDec (take 8 xs))] ++ decode (drop 9 xs)

toDec :: [Int] -> Int
toDec = foldl' (\acc x -> acc * 2 + x) 0   
