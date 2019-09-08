module Src1 where

-- Problem 1


myLast :: [a] -> a
myLast [] = error "myLast : empty list"
myLast (x:xs) | null xs = x
              | otherwise = myLast xs
    

-- Problem 2

myButLast :: [a] -> a
myButLast [] = error "empty list"
myButLast (x:xs)    | length xs == 1 = x
                    | null xs = error "list with just one element"
                    | otherwise = myButLast xs


elementAt :: [a] -> Int -> a
elementAt [] _ = error "empty list or input n >= length of list"
elementAt (x:xs) n  | n == 0 = x
                    | otherwise = elementAt xs (n-1)


myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs


myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = error "Empty list"
isPalindrome xs = xs == myReverse xs


compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = x : compressHelper xs x
    where compressHelper (x:xs) c = if x == c 
                                    then compressHelper xs c
                                    else x : compressHelper xs x
          compressHelper _ _ = []


data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concatMap flatten xs 




pack :: (Eq a) => [a] -> [[a]] 
pack [] = []
pack (x:xs) = packHelper xs [[x]]
    where packHelper (x:xs) ys =    if head (last ys) == x
                                    then packHelper xs (init ys ++ [(last ys ++ [x])])
                                    else packHelper xs (ys ++ [[x]])
          packHelper _ ys = ys



encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) = encodeHelper xs [(1, x)]
    where encodeHelper (x:xs) ys = if snd (last ys) == x
                                   then encodeHelper xs (init ys ++ [(1 + fst (last ys), x)])
                                   else encodeHelper xs (ys ++ [(1, x)])
          encodeHelper _ ys = ys



-- Problem 10

