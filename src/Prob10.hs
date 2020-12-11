module Prob10 (prob10 )where

import Data.String.Utils
import System.IO


readData :: String -> IO [Int]
readData fileName = do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    let linesInFile = lines contents
        trimmed = fmap rstrip linesInFile :: [String]
        nums = map (\s -> (read s :: Int)) trimmed
        _ = hClose handle
    print ("num trimmed lines: " ++ show (length trimmed))
    print ("first line in file: " ++ head trimmed)
    return nums
    

-- Each of your joltage adapters is rated for a specific output joltage (your puzzle input). 
-- Any given adapter can take an input 1, 2, or 3 jolts lower than its rating and still produce its rated output joltage.
-- your device has a built-in joltage adapter rated for 3 jolts higher than the highest-rated adapter in your bag.
prob10 :: IO ()
prob10 = do
    print "hi"
