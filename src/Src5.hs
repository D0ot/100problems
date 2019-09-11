module Src5 where 

import Data.List

and' :: Bool -> Bool -> Bool
True `and'` True = True
_ `and'` _ = False

or' :: Bool -> Bool -> Bool
False `or'` False = False
_ `or'` _ = True

not' :: Bool -> Bool
not' True = False
not' False = True

nand' :: Bool -> Bool -> Bool
x `nand'` y = not'$ and' x y

nor' :: Bool -> Bool -> Bool
x `nor'` y = not' $ or' x y

xor' :: Bool -> Bool -> Bool 
x `xor'` y  | x == y = False
            | otherwise = True

infixl 7 `equ'`
equ' :: Bool -> Bool -> Bool
x `equ'` y = not $ x `xor'` y

table :: (Bool -> Bool -> Bool) -> String
table f = show $ map (\(x,y) -> (x, y,f x y)) allPosible
    where allPosible = [(i, j) | i <- tmp, j <- tmp]
          tmp = [True, False]

--48
genMixList :: [[a]] -> [a] -> [[a]]
genMixList xs = concatMap f
    where f = genMixEle xs

genMixEle :: [[a]] -> a -> [[a]]
genMixEle xs t = map f xs
    where f x = x ++ [t]

genTfTable :: Int -> [[Bool]]
genTfTable 0 = [[]]
genTfTable n = genMixList (genTfTable (n-1)) [True, False]

tablen :: Int -> ([Bool] -> Bool) -> [[Bool]]
tablen n f = map h (genTfTable n)
    where h xs = xs ++ [f xs]

prettyPrintTable :: [[Bool]] -> String
prettyPrintTable = concatMap f
    where f x = show x ++ "\n"


--49 Gray codes
grayCodes :: Int -> [String]
grayCodes n = nTake $ transpose $ map singleColumn (reverse [1..n])
    where singleColumn c = replicate (2^(c-1)) '0'++ concat (repeat (replicate (2^c) '1' ++ replicate (2^c) '0'))
          nTake = take (2^n)

          

          
--50 huffman codes

data HuffmanTree = HTNode Int HuffmanTree HuffmanTree | HTLeaf (Char, Int) | HTEmpty
                   deriving (Show, Eq)

huffman :: [(Char, Int)] -> [(Char, String)]
huffman xs = huffmanCode "" (huffmanBase $ map HTLeaf xs)

huffmanBase :: [HuffmanTree] -> HuffmanTree
huffmanBase [x,y] = if xf > yf
                    then HTNode sumf y x
                    else HTNode sumf x y
                    where xf = huffmanGetf x
                          yf = huffmanGetf y
                          sumf = xf + yf
huffmanBase xs = huffmanBase (newNode : rest)
    where ([min1, min2], rest) = huffmanTakeMin2 xs 
          newNode = HTNode (huffmanFplus min1 min2) min1 min2

huffmanCode :: String -> HuffmanTree -> [(Char, String)]
huffmanCode p (HTNode _ l r) = concat [huffmanCode (p++"0") l, huffmanCode (p++"1") r]
huffmanCode p (HTLeaf (c, f)) = [(c, p)]
          

huffmanTakeMin2 :: [HuffmanTree] -> ([HuffmanTree],[HuffmanTree])
huffmanTreeMin2 [x, y] = ([x, y], [])
huffmanTakeMin2 xs = minTwo
    where sortHelper h1 h2 = compare (huffmanGetf h1) (huffmanGetf h2)
          sorted = sortBy sortHelper xs
          minTwo = splitAt 2 sorted


huffmanGetf :: HuffmanTree -> Int
huffmanGetf (HTNode x _ _) = x
huffmanGetf (HTLeaf (_, x)) = x

huffmanFplus :: HuffmanTree -> HuffmanTree -> Int
huffmanFplus x y = (huffmanGetf x) + (huffmanGetf y)