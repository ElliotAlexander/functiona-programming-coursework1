
neighbours :: (Floating a, Ord a) => Int -> (a,a) -> [(a,a)] -> [(a,a)]
neighbours k (p,q) x:xs
    | xs == [] = []
    | otherwise = compare_coords (x,y) (c,d) ++ neighbours (x,y) xs


    -- Distance from K -> P
compare_coords :: (Floating a, Ord a) => Int -> (a,a) -> (a,a) -> Float
compare_coords (x,y) (a,b) = sqrt((^ (x-a) 2) +  (^ (y-b) 2))