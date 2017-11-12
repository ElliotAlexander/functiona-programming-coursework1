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
