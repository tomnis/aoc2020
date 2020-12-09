module Prob6 (prob6) where

import Data.List
import Data.List.Utils
import Data.String.Utils
import System.IO

readData :: String -> IO [[String]]
readData fileName = do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    let groups = split "\n\n" contents :: [String]
        trimmed = fmap rstrip groups :: [String]
        people = fmap (split "\n") trimmed :: [[String]]
        _ = hClose handle

    return people

-- accept a [String] containing responses for each person in group
numQuestionsYes :: [String] -> Integer
numQuestionsYes answers = toInteger (length (nub (join "" answers)))

part1 :: [[String]] -> Integer
part1 s = sum (fmap numQuestionsYes s)

-- intersection
allAnsweredYes :: [String] -> Integer
allAnsweredYes answers = toInteger (length (foldr1 intersect answers))

part2 :: [[String]] -> Integer
part2 s = sum (fmap allAnsweredYes s)

prob6 :: IO ()
prob6 = do
    rows <- readData "inputs/prob6.txt"
    print (head rows)
--    print (rows !! 1)
    print (foldr1 intersect (head rows))
--    print (part1 rows)
    -- 3112 too low
    print (part2 rows)
--    print (("abc" `intersect` "cdef") `intersect` "cdefg")
--    print (foldl1 intersect ["abc", "cde", "cba"])
    print (foldl1 intersect ["abc"])