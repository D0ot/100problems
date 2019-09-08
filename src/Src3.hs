module Src3 where


insertAt :: a -> [a] -> Int -> [a]
insertAt _ [] _  = []
insertAt c xs 0 = c : xs
insertAt c (x:xs) n = x : insertAt c xs (n-1)


range :: (Enum a, Eq a) => a -> a -> [a]
range x1 y1 | x1 /= y1 = x1 : range (succ x1) y1
            | otherwise = [y1]


