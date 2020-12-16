module Prob9(prob9) where

import Data.List
import Data.Maybe
import Data.String.Utils
import qualified Data.HashSet as HashSet
import System.IO
import Util



readData :: String -> IO [Integer]
readData fileName = do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    let linesInFile = lines contents
        trimmed = fmap rstrip linesInFile :: [String]
        nums = map (\s -> (read s :: Integer)) trimmed
        _ = hClose handle
    print ("num trimmed lines: " ++ (show (length trimmed)))
    print ("first line in file: " ++ (head trimmed))
    return nums

sumToTarget :: [Integer] -> Integer -> Bool
sumToTarget nums target =
    let numSet = HashSet.fromList nums
        containedNums = filter p nums
            where p x = HashSet.member (target - x) numSet
    in (length containedNums) > 0





-- first int that is not a sum of any of the previous 25 nums
-- take sliding windows of 26 and find the first that is false
part1 :: [Integer] -> Integer
part1 nums =
    let windows = slidingWindows 26 nums :: [[Integer]]
        noSum = find p windows :: Maybe [Integer]
            where p window = not (sumToTarget (init window) (last window))
        l = fmap last noSum
    in unbox (-1) l



contiguousSetSum :: [Integer] -> Integer -> [Integer]
contiguousSetSum nums target =
    let ts = tails nums :: [[Integer]]
        candidates = concat (fmap inits ts) :: [[Integer]]
        soln = find (\group -> (sum group) == target) candidates :: Maybe [Integer]
    in head (maybeToList soln)

-- find a contiguous set of at least two numbers in your list which sum to the invalid number from step 1
-- add together the smallest and largest number in this contiguous range
part2 :: [Integer] -> Integer -> Integer
part2 nums target =
    let contig = contiguousSetSum nums target :: [Integer]
    in (minimum contig) + (maximum contig)

prob9 :: IO ()
prob9 = do
    nums <- readData "inputs/prob9.txt"
    print ("num nums: " ++ (show (length nums)))
    print ("num1: " ++ (show (head nums)))
    print ("last num: " ++ (show (last nums)))
    let p1 = part1 nums :: Integer
    print ("part1 result: " ++ (show p1))
    let p2 = part2 nums p1 :: Integer
    print ("part2 result: " ++ (show p2))


