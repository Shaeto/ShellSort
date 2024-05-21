{-# LANGUAGE Safe #-}

{- | Shellsort - in-place comparison sort
    https://en.wikipedia.org/wiki/Shellsort
-}

module Shellsort where

-- | sort elements using a list of gaps provided ba caller
shellSort :: (Ord a) => [a] -> [Int] -> [a]
shellSort l gaps =
    let len = length l
    in foldl insertionSort l [x | gap <- gaps, x <- [ [n, (n + gap) .. (len - 1)] | n <- [ 0.. (gap - 1)]] ]

-- | sort elements using a standard list of gaps suggested by Shell
shellSort1 :: (Ord a) => [a] -> [a]
shellSort1 l = shellSort l $ gapsShell $ length l

-- | sub-list insertion sort
-- sub-list is defined by list of indexes
insertionSort :: (Ord a) => [a] -> [Int] -> [a]
insertionSort [] _ = []
insertionSort l [] = l
insertionSort l li =
    fst $ foldl (\acc x -> let lp = snd acc ++ [x] in (sortLastPair (fst acc) lp, lp)) (l, []) (zip li $ drop 1 li)

-- | moves unsorted element defined by last pair to correct position in the sorted part of the (sub)list
sortLastPair :: (Ord a) => [a] -> [(Int, Int)] -> [a]
sortLastPair l [] = l
sortLastPair l lp
    | item1 > item2 = sortLastPair (swapItems l pair) (init lp)
    | otherwise = l
    where
        item1 = l !! fst pair
        item2 = l !! snd pair
        pair = last lp

-- | exchange two elements of the list
swapItems :: [a] -> (Int, Int) -> [a]
swapItems l (i1, i2)
    | i1 == i2 = l
    | i1 > i2 = swapItems l (i2, i1)
    | otherwise = take i1 l ++ [item2] ++ take (i2 - i1 - 1) (drop (i1+1) l) ++ [item1] ++ drop (i2 + 1) l
        where
            item1 = l !! i1
            item2 = l !! i2

-- Gap sequences

-- | [N/2^k], author Shell, 1959
gapsShell :: Int -> [Int]
gapsShell s =  takeWhile (>0) [ floor $ (fromIntegral s :: Double) / 2^(k::Int) | k <- [1..]]

-- | 2*[N/2^(k+1)] + 1, authors Frank & Lazarus, 1960
gapsFrankLazaus :: Int -> [Int]
gapsFrankLazaus s =  takeWhile (>1) [ 2 * (floor $ (fromIntegral s :: Double) / 2^(k::Int)) + 1 | k <- [1..]] ++ [1]

-- | 2^k - 1, author Hibbard, 1963
gapsHibbard :: Int -> [Int]
gapsHibbard s =  reverse $ takeWhile (<s) [ 2^(k::Int) - 1 | k <- [1..]]

-- | experimentally derived, author Ciura, 2001
gapsCiura :: [Int]
gapsCiura = [701, 301, 132, 57, 23, 10, 4, 1]
