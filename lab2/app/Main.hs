module Main (main) where

import StringAlignment

main :: IO ()
main = do
    -- putStrLn "String 1: "
    -- str1 <- getLine
    -- putStrLn "String 2: "
    -- str2 <- getLine
    -- putStrLn $ "Similarity score: " ++ show (similarityScore str1 str2)
    putStrLn $ "Optimal alignments: " ++ show (optAlignments string1 string2)