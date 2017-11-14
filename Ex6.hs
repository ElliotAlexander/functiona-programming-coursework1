import Data.List

-- This seems to work wonderfully

data DistancePoint = DistancePoint {distance :: Float, coord :: (Float, Float)} deriving (Eq, Ord, Show)
neighbours :: (Floating a, Ord a) => Int -> (a,a) -> [(a,a)] -> [(a,a)]
neighbours k (p,q) xs = take k [x | (_,x) <- sort([ (sqrt((p-r)^2 + (q-s)^2), (r,s)) | (r,s) <- xs])]
