import Data.List

-- ODn't include this

makeChange :: Int -> [Int] -> [(Int, Int)]
makeChange remaining denoms
    | getChange remaining denoms == [] = replicate (length denoms) (-1,-1)
    | otherwise = frequency (getChange remaining denoms)

getChange :: Int -> [Int] -> [Int]
getChange 0  denoms = []
getChange _ [] = [0]
getChange remaining denoms@(d1:d2) 
    | remaining >= d1 = head(map ( d1:) [(getChange (remaining - d1) denoms)])
    | otherwise = getChange remaining d2 

frequency :: [Int] -> [(Int,Int)] 
frequency list = map (\l -> (length l, head l)) (group (sort list))