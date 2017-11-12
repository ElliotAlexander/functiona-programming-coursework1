import Data.List

-- COMPLETE

subtotal :: Num a => [a] -> [a]
subtotal [] = []
subtotal (x:[]) = [x]
subtotal (x:y:xs) =  [x] ++ subtotal (xs ++ [x+y])
