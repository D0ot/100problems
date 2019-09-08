module Src3 where

import System.Random

insertAt :: a -> [a] -> Int -> [a]
insertAt _ [] _  = []
insertAt c xs 0 = c : xs
insertAt c (x:xs) n = x : insertAt c xs (n-1)


range :: (Enum a, Eq a) => a -> a -> [a]
range x1 y1 | x1 /= y1 = x1 : range (succ x1) y1
            | otherwise = [y1]


-- skip from 23 to 25

-- problem 26

combinations :: Int -> [a] -> [[a]]
combinations n xs = if length xs <= n 
                    then [xs] 
                    else stepRet ++ combinations n (tail xs)
                    where   header = take (n-1) xs
                            stepRet = map (\x -> header ++ [x]) (drop (n-1) xs)

-- problem 27
-- dont know how to do it

nameList :: [String]
nameList = ["aldo", "beat", "carla", "david", "evi", "flip", "gary", "hugo", "ida"]


group :: [Int] -> [a] -> [[[a]]]
group ns xs = case checkInput of
    True -> []
    False -> error "two many group"
    where checkInput = sum ns < length xs


-- skip problem 28