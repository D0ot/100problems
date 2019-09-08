module Src2 where


import Src1
-- probleam 11

data Counter a = Single a | Multiple Int a
               deriving (Show, Eq)

increaseCounter (Single a) = Multiple 2 a
increaseCounter (Multiple n a) = Multiple (n+1) a

getElemInCounter (Single a) = a
getElemInCounter (Multiple _ a) = a

encodeModified xs = map f $ Src1.encode xs
    where f (1, c) = Single c
          f (n, c) = Multiple n c

decodeModified :: [Counter a] -> [a]
decodeModified = concatMap f
    where f (Single c) = [c]
          f (Multiple n c) = replicate n c


encodeDirect :: (Eq a) => [a] -> [Counter a]
encodeDirect (x:xs) = encDirHelper xs [Single x]
    where encDirHelper (x:xs) ys    | getElemInCounter (last ys) == x = encDirHelper xs (init ys ++ [increaseCounter (last ys)])
                                    | otherwise = encDirHelper xs (ys ++ [Single x])
          encDirHelper _ ys = ys 



dupli' :: [a] -> [a]
dupli' = concatMap (\x -> replicate 2 x)


myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ myConcat xs

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = (f x) : (myMap f xs)

myConcatMap f xs = myConcat (myMap f xs)

dupli :: [a] -> [a]
dupli = myConcatMap (\x -> replicate 2 x)

repli :: [a] -> Int -> [a]
repli xs n = concatMap (\x -> replicate n x) xs


dropEvery :: [a] -> Int -> [a]
dropEvery xs e = dropEveryHelper xs 1 
    where dropEveryHelper (x:xs) n =    if (n `rem` e) == 0
                                        then dropEveryHelper xs (n+1)
                                        else x : dropEveryHelper xs (n+1)
          dropEveryHelper _ _ = []

myTake :: [a] -> Int -> [a]
myTake [] _ = []
myTake _ 0 = []
myTake (x:xs) n = x : myTake xs (n-1)

myDrop :: [a] -> Int -> [a]
myDrop [] _ = []
myDrop xs 0 = xs
myDrop (x:xs) n = myDrop xs (n-1)


split :: [a] -> Int -> ([a], [a]) 
split xs n = (myTake xs n, myDrop xs n)


slice :: [a] -> Int -> Int -> [a]
slice xs p1 p2 = myTake (myDrop xs (p1-1)) (p2 - p1 + 1) 


rorate :: [a] -> Int -> [a]
rorate (x:xs) n = if toLeft == 0
                  then x:xs
                  else rorate (xs ++ [x]) (n-1)
        where   len = length xs
                toLeft = fun1 n 
                    where fun1 n =  if n >= 0
                                    then n
                                    else fun1 (n + len)
rorate _ _ = []

removeAt :: Int -> [a] -> [a]
removeAt 0 (x:xs) = xs
removeAt n (x:xs) = x : removeAt (n-1) xs
removeAt _ _ = []