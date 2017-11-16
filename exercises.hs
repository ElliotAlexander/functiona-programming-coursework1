import Data.Char
import Data.List



-- Exercise 1
subtotal :: Num a => [a] -> [a]
subtotal [] = []
subtotal (x:[]) = [x]
subtotal (x:y:xs) =  [x] ++ subtotal (xs ++ [x+y])


  -- Exercise 2
histogram :: Int -> [Int] -> [Int]
histogram n xs
    | xs == [] = []
    | otherwise = [length y] ++ histogram (2*n) (xs \\ y)
    where 
        y =  [x | x <- xs, x <= n-1]


  -- Exercise 3
meetsOffer :: String -> Int -> Bool
meetsOffer s i
    | (meetsOffer' s) > i = True
    | otherwise = False
  
  
meetsOffer' :: String -> Int
meetsOffer' s
    | s == [] = 0
    | take 2 s == "A*" = 56 + meetsOffer' (drop 2 s)
    | head s == 'A' = 50 + meetsOffer' (drop 1 s)
    | head s == 'B' = 40 + meetsOffer' (drop 1 s)
    | head s == 'C' = 32 + meetsOffer' (drop 1 s)

    
-- Exercise 4


data TypeOfSort = Ascending | NonDescending | Constant | NonAscending | Descending | NotSorted deriving Show
sortType :: Ord a => [a] -> TypeOfSort
sortType xs
  | xs == [] = Ascending
  | length minusdups == 1 = NonAscending
  | sort xs == xs && dup == True = Ascending
  | reverse (sort xs) == xs && dup == True = Descending
  | sort xs == xs = NonDescending
  | reverse (sort xs) == xs = NonAscending
  | otherwise = NotSorted
  where 
    minusdups = map head (group(sort xs))
    dup = length minusdups == length xs

    -- Exercise 5

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


-- Exercise 6

data DistancePoint = DistancePoint {distance :: Float, coord :: (Float, Float)} deriving (Eq, Ord, Show)
neighbours :: (Floating a, Ord a) => Int -> (a,a) -> [(a,a)] -> [(a,a)]
neighbours k (p,q) xs = take k [x | (_,x) <- sort([ (sqrt((p-r)^2 + (q-s)^2), (r,s)) | (r,s) <- xs])]


-- Exercise 7

data SearchTree = Node SearchTree Int SearchTree | Leaf Int deriving Show

--balanced :: SearchTree -> Int -> Boolean
--balanced tree current_depth
--    | isNode tree == True = 
--    | otherwise = 


--checknode :: SearchTree -> Int -> Bool
--checknode (Node _ _ _) tree  = False
--checknode (Leaf _) leaf x = True


--isNode :: SearchTree -> Bool
--isNode (Node _ _ _ ) = True
--isNode _ = False


-- Exercise 8

-- SEEMINGLY WORKING
-- NEEDS MORE TESTING

newtonRootSequence :: Double -> [Double]
newtonRootSequence d = newtonRootSequence' d 1.0


newtonRootSequence' :: Double -> Double -> [Double]
newtonRootSequence' d xn = [((xn+d/xn)/2)]  ++ newtonRootSequence' d ((xn + d / xn) /2)

newtonRoot :: Double -> Double -> Double
newtonRoot d epsilon = newtonRoot' d epsilon 1


newtonRoot' :: Double -> Double -> Double -> Double
newtonRoot' d epsilon xn
    | abs (xn - xnPlusOne) > epsilon = newtonRoot' d epsilon xnPlusOne
    | otherwise = xn
    where
        xnPlusOne = ((xn + d) / xn)/2
    
-- Exercise 9

hyperOperator :: Int -> Int -> Int -> Int
hyperOperator n a b 
    | n == 0 = b + 1
    | b == 0 = hyperOperator' n a
    | otherwise = hyperOperator (n-1) a (hyperOperator n a (b-1))

hyperOperator' :: Int -> Int -> Int
hyperOperator' n a
    | n == 1 = a
    | n == 2 = 0
    | n >= 3 = 1


-- Exercise 10

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


-- Exercise 11

decode :: [Int] -> String
decode xs
    | xs == [] = []
    | otherwise = [chr (toDec (take 8 xs))] ++ decode (drop 9 xs)

toDec :: [Int] -> Int
toDec = foldl' (\acc x -> acc * 2 + x) 0   

-- Exercise 12 - close but not quite working

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

-- Exercise 15


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
