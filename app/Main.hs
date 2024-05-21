{-# LANGUAGE Safe #-}

module Main where
import safe System.Random ( getStdGen, Random(randomRs) )
import safe Shellsort

main :: IO ()
main = do
    -- generate a test set
    gen <- getStdGen
    let xs = take 50 $ randomRs (1, 100) gen :: [Int]

    -- print a test set
    putStrLn "Test set is:"
    putStrLn $ show xs

    -- print a sorted test set
    putStrLn $ "Shell sort using Shell gaps: " ++ (show $ gapsShell $ length xs)
    putStrLn $ show $ shellSort xs $ gapsShell $ length xs
    putStrLn ""

    putStrLn $ "Shell sort using Hibbard gaps: " ++ (show $ gapsHibbard $ length xs)
    putStrLn $ show $ shellSort xs $ gapsHibbard $ length xs
    putStrLn ""

    putStrLn $ "Shell sort using Frank & Lazaus gaps: " ++ (show $ gapsFrankLazaus $ length xs)
    putStrLn $ show $ shellSort xs $ gapsFrankLazaus $ length xs
    putStrLn ""

    putStrLn $ "Shell sort using Ciura gaps: " ++ (show gapsCiura)
    putStrLn $ show $ shellSort xs $ gapsCiura
    putStrLn ""
