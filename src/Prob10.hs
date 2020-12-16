module Prob10 (prob10 )where

import Data.String.Utils
import System.IO
import Util


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


-- number of 1-jolt differences and 3-jolt differences
countJoltageDifferences :: [Int] -> (Int, Int)
countJoltageDifferences chain =
    foldl facc (0, 0) (slidingWindows 2 chain)


facc :: (Int, Int) -> [Int] -> (Int, Int)
facc (acc1, acc3) (a:b:[])
    | diff == 1 = (acc1 + 1, acc3)
    | diff == 2 = (acc1, acc3)
    | diff == 3 = (acc1, acc3 + 1)
    | otherwise = error ("diff: " ++ (show (a, b)))
    where diff = b - a
facc _ n = error ("unsupported" ++ (show n))

part1 :: [Int] -> Int
part1 adapters =
    let deviceJoltage = (maximum adapters) + 3
        -- add a 0 to account for the charging outlet
        chain = quicksort (0:deviceJoltage:adapters)
        (jolt1diffs, jold3diffs) = countJoltageDifferences chain
    in jolt1diffs * jold3diffs




canRemoveAdapter :: [Int] -> Bool
canRemoveAdapter (a:b:c:[]) = (c - a) <= 3
canRemoveAdapter ns = error ("length must be 3 but got: " ++ (show (length ns)))




-- What is the total number of distinct ways you can arrange the adapters to connect the charging outlet to your device?
part2 :: [Int] -> Integer
part2 adapters =
    let deviceJoltage = (maximum adapters) + 3
        chain = quicksort (0:deviceJoltage:adapters)
        adaptersCanBeRemoved = count canRemoveAdapter (slidingWindows 3 chain)
    in (2 ^ adaptersCanBeRemoved)




-- scan ahead counting the number of entries until cumulative diff >= 3
-- once the cumulative diff is >= 3, check the number of
-- cumulative diff == 3, we can remove any of the subsequence elements independently, at most 2?
-- cumulative diff > 3, ??? consider 0,1,2,4, we have to start over from 1?

-- (0), 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19, (22)
-- (0), 1, 4, 5, 6, 7, 10, 12, 15, 16, 19, (22)
-- (0), 1, 4, 5, 7, 10, 11, 12, 15, 16, 19, (22)
-- (0), 1, 4, 5, 7, 10, 12, 15, 16, 19, (22)
-- (0), 1, 4, 6, 7, 10, 11, 12, 15, 16, 19, (22)
-- (0), 1, 4, 6, 7, 10, 12, 15, 16, 19, (22)
-- (0), 1, 4, 7, 10, 11, 12, 15, 16, 19, (22)
-- (0), 1, 4, 7, 10, 12, 15, 16, 19, (22)

-- Each of your joltage adapters is rated for a specific output joltage (your puzzle input). 
-- Any given adapter can take an input 1, 2, or 3 jolts lower than its rating and still produce its rated output joltage.
-- your device has a built-in joltage adapter rated for 3 jolts higher than the highest-rated adapter in your bag.
prob10 :: IO ()
prob10 = do
    print "hi"
    nums <- readData "inputs/prob10.txt"
    print ("num nums: " ++ (show (length nums)))
    print ("num1: " ++ (show (head nums)))
    print ("last num: " ++ (show (last nums)))
    print (slidingWindows 2 nums)
    -- 2010 too low
    print (quicksort nums)
    let p1 = part1 nums :: Int
    print ("part1 result: " ++ (show p1))
    -- 562949953421312 too high
    -- 140737488355328 too high
    let p2 = part2 nums :: Integer
    print ("part2 result: " ++ (show p2))
