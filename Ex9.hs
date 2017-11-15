-- Attempted

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