
--neighbours :: (Floating a, Ord a) => Int -> (a,a) -> [(a,a)] -> [(a,a)]
--neighbours k (p,q) (x,y):xs
--    | xs == [] = []
--    where
--        allneighbours = 
--        kneighbours = take 


getdistance :: (Floating a, Ord a) => (a,a) -> (a,a) -> (a,a)
getdistance (p,q) (x,y) = sqrt ((^ (p-x) 2) + (^ (q - y) 2))

