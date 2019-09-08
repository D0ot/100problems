module Src4 where

-- 31

isPrime :: Int -> Bool
isPrime 1 = True
isPrime n = all (\x -> n `mod` x /= 0) factorList
    where factorList = init [2..n]


-- 32

myGcd :: Int -> Int -> Int
myGcd a 0 = a
myGcd a b = myGcd b (a `mod` b)


-- 33

coprime :: Int -> Int -> Bool
coprime a b = myGcd a b == 1



-- 34
totient :: Int -> Int
totient m = foldr f 0 (init [1..m])
    where f x acc = if coprime x m then acc + 1 else acc


-- 35
primeFactors :: Int -> [Int]
primeFactors m = priFacHper 2 m
    where priFacHper i res  | i > m = []
                            | isPrime i && isModular0 = i : priFacHper i (res `div` i)
                            | otherwise = priFacHper (i+1) res
                                where isModular0 = res `mod` i == 0
-- 36
primeFactorMult :: Int -> [(Int, Int)]
primeFactorMult m = pastProcess $ primeFactors m
    where pastProcess (x:xs) = foldl f [(x,1)] xs
          f acc x = if fst (last acc) == x
                    then init acc ++ [(x, snd (last acc) + 1)]
                    else acc ++ [(x, 1)]


-- 37

newTotient :: Int -> Int
newTotient m = newTotHelper factorList
    where factorList = primeFactorMult m
          newTotHelper ((p1, m1):ps) = (p1-1) * p1 ^ (m1-1) * newTotHelper ps
          newTotHelper _ = 1

--38 
-- totient is faster than newTotient

--39

primeR :: Int -> Int -> [Int]
primeR p1 p2 = filter (\x -> odd x && isPrime x) [p1..p2]


goldbach :: Int -> (Int, Int)
goldbach m = map (\x y-> )
    where primeNumbers = primerR 3 (m-3)
