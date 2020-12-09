module Prob5(prob5) where

import System.IO
import Util

readData :: String -> IO [String]
readData fileName = do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    let linesInFile = lines contents
        _ = hClose handle

    return linesInFile

mid :: Int -> Int -> Int
mid low hi = low + ((hi - low) `div` 2)

findRow :: String -> Int -> Int -> Int
findRow [] low hi = low
-- F means take low half
findRow ('F':spec) low hi = findRow spec low (mid low hi)
findRow ('B':spec) low hi = findRow spec (mid low hi) hi
findRow (x:xs) low hi = -1

findSeat :: String -> Int -> Int -> Int
findSeat [] low hi = low
-- L means take low half
findSeat ('L':spec) low hi = findSeat spec low (mid low hi)
-- R means take upper half
findSeat ('R':spec) low hi = findSeat spec (mid low hi) hi

seatId :: Int -> Int -> Int
seatId row seat = (row * 8) + seat

seatIdStr :: String -> Int
seatIdStr s =
    let rowSpec = take 7 s
        seatSpec = drop 7 s
        row = findRow rowSpec 0 128
        seat = findSeat seatSpec 0 8
    in seatId row seat

part1 :: [String] -> Int
part1 seatSpecs = maximum (fmap seatIdStr seatSpecs)


-- assumed sorted
findMissing :: [Int] -> Int
findMissing [] = error "missing in empty list"
findMissing (a:b:ids)
    | (b - a) > 1 = a + 1
    | otherwise = m
    where m = findMissing (b:ids)


part2 :: [Int] -> Int
part2 seatIds =
    let sortedIds = quicksort seatIds
    in findMissing sortedIds



prob5 :: IO ()
prob5 = do
    rows <- readData "inputs/prob5.txt"
--    print (head rows)
    print (findRow "FBFBBFF" 0 128)
    print (findSeat "RLR" 0 8)
    print (seatId 44 5)
    print (part1 rows)
    print (part2 (fmap seatIdStr rows))