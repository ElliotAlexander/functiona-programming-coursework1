import Data.List

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