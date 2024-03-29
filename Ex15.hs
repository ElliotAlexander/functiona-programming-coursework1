--isCantorPair
-- Working

isCantorPair :: Int -> Bool
isCantorPair p = tail pair == [(sum xypair)]
    where 
        pair = cantorReverse p
        xypair = cantorReverse (head pair)
    

cantorFunction :: Int -> Int -> Int
cantorFunction x y
    | x == 0 = 0
    | y == 0 = 0
    | otherwise = (((x + y) * (x + y + 1)) `div` 2 ) + y

cantorReverse ::  Int -> [Int]
cantorReverse z
    | otherwise = [a,b]
    where 
        t = floor (((sqrt (8 * (fromIntegral z) + 1)) - 1) / 2)
        a = (t * (t + 3)) `div` 2 - z
        b = z - ((t * (t + 1)) `div` 2)
