module Prob1 (prob1) where

import System.IO
import qualified Data.HashSet as HashSet

readData :: String -> IO [Int]
readData fileName = do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    let linesInFile = lines contents
        nums = map (\n -> read n :: Int) linesInFile
        _ = hClose handle
    return nums


prob1 :: IO ()
prob1 = do
    nums <- readData "inputs/prob1.txt"
    let t = part1 nums 2020
    print t
    let t2 = part2 nums 2020
    print t2


part1 :: [Int] -> Int -> Int
part1 nums target =
    let hashNums = HashSet.fromList nums
        containedNum = head (filter p nums)
            where p x = HashSet.member (target - x) hashNums
    in containedNum * (target - containedNum)

part2 :: [Int] -> Int -> Int
part2 nums target =
    let hashNums = HashSet.fromList nums
        cartProd = [ (x, y) | x <- nums, y <- nums ]
        (n1, n2) = head (filter p cartProd)
            where p (x, y) = HashSet.member (target - (x + y)) hashNums
    in n1 * n2 * (target - (n1 + n2))
