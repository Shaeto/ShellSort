{-# LANGUAGE Safe #-}

{- | Shellsort - in-place comparison sort
    https://en.wikipedia.org/wiki/Shellsort
-}

module Shellsort where

-- | sort elements using a list of gaps provided ba caller
shellSort :: (Ord a) => [Int] -> [a] -> [a]
shellSort gaps l =
    let len = length l
    in foldl (flip insertionSort) l $ gapsToSublists len gaps

-- | generate indexes of all possible sublists
gapsToSublists :: Int -> [Int] -> [[Int]]
gapsToSublists len gaps =
    [ x | gap <- gaps, x <- [ [n, (n + gap) .. (len - 1)] | n <- [ 0.. (gap - 1)]] ]

-- | sub-list insertion sort
-- sub-list is defined by list of indexes
insertionSort :: (Ord a) => [Int] -> [a] -> [a]
insertionSort _ [] = []
insertionSort [] l = l
insertionSort li l =
    snd $ foldl (\acc x -> let lp = fst acc ++ [x] in (lp, sortLastPair lp (snd acc))) ([], l) (zip li $ drop 1 li)

-- | moves unsorted element defined by last pair to correct position in the sorted part of the (sub)list
sortLastPair :: (Ord a) => [(Int, Int)] -> [a] -> [a]
sortLastPair [] l = l
sortLastPair lp l
    | item1 > item2 = sortLastPair (init lp) (swapItems pair l)
    | otherwise = l
    where
        item1 = l !! fst pair
        item2 = l !! snd pair
        pair = last lp

-- | exchange two elements of the list
swapItems :: (Int, Int) -> [a] -> [a]
swapItems (i1, i2) l
    | i1 == i2 = l
    | i1 > i2 = swapItems (i2, i1) l
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
