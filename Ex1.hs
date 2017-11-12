import Data.List

subtotal :: Num a => [a] -> [a]
subtotal [] = []
subtotal (x:xs) =  [x] ++ subtotal (xs ++ [x])
