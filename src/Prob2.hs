module Prob2 (prob2) where

import System.IO
import Text.Regex.TDFA
import Util

data Policy = Policy { lowerBound :: Int, upperBound :: Int, ch :: Char } deriving (Show)
--4-5 w: wwwxr
data Row = Row { policy :: Policy, password :: String } deriving (Show)


readData :: String -> IO [Row]
readData fileName = do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    let linesInFile = lines contents
        rows = map parseRow linesInFile
        _ = hClose handle
    return rows


parseRow :: String -> Row
parseRow s =
    let regex = "\\`([0-9]+)-([0-9]+) ([a-z]): ([a-zA-Z]+)\\'"
        (_, _, _, groups) = s =~ regex :: (String, String, String, [String])
        lowerBound = read (head groups) :: Int
        upperBound = read (groups !! 1) :: Int
        ch = head (groups !! 2)
        password = groups !! 3
    in Row {policy=Policy { lowerBound=lowerBound, upperBound=upperBound, ch=ch}, password=password}

isValid :: Row -> Bool
isValid row =
    let
        matchingChars = filter p (password row)
            where p c = c == ch (policy row)
        count = length matchingChars
        low = lowerBound (policy row)
        up = upperBound (policy row)
    in (low <= count) && (count <= up)



isValid2 :: Row -> Bool
isValid2 row =
    let
        pass = password row
        pol = policy row
        --dont use 0-based indexing
        p1 = lowerBound pol - 1
        p2 = upperBound pol - 1
        c = ch pol
    in xor ((pass !! p1) == c) ((pass !! p2) == c)

--count
part1 :: [Row] -> Int
part1 = count isValid

part2 :: [Row] -> Int
part2 = count isValid2

prob2 :: IO ()
prob2 = do
    rows <- readData "inputs/prob2.txt"
    print (head rows)
    let p1 = part1 rows
    print p1
    let p2 = part2 rows
    print p2
