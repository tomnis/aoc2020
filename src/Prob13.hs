module Prob13 (prob13) where

import Data.Int
import Data.List
import Data.Maybe
import Data.String.Utils
import System.IO
import Util


type Timestamp = Int
type BusId = String

readData :: String -> IO (Timestamp, [BusId])
readData fileName = do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    let linesInFile = lines contents
        trimmed = fmap rstrip linesInFile :: [String]
        timestamp = read (head trimmed) :: Int
        ids = split "," (trimmed !! 1) :: [BusId]
        _ = hClose handle
    return (timestamp, ids)


-- ID of the earliest bus you can take to the airport multiplied by the number of minutes you'll need to wait for that bus?
part1 :: Timestamp -> [Int] -> Int
part1 timestamp ids =
    let times = map (\id -> (id - (timestamp `mod` id))) ids
        pairs = zip ids times :: [(Int, Int)]
        (bus, waitTime) = minimumBy compareWaitTimes pairs
    in bus * waitTime


compareWaitTimes :: (Int, Int) -> (Int, Int) -> Ordering
compareWaitTimes (_, w1) (_, w2) = if w1 < w2 then LT else GT


------------------------------------------------
------------------------------------------------
------------------------------------------------

-- earliest timestamp such that the first bus ID departs at that time and each subsequent listed bus ID departs at that subsequent minute
part2 :: [Int] -> Int64
part2 _ = -1


buildConstraints :: [(Int, String)] -> [(Int, Int)]
buildConstraints [] = []
buildConstraints ((i,"x"):cs) = buildConstraints cs
buildConstraints ((i,n):cs) = (i, read n :: Int):(buildConstraints cs)


prob13 :: IO ()
prob13 = do
    print "hi"
    (timestamp, ids) <- readData "inputs/prob13.txt"
    print ("timestamp: " ++ show (timestamp))
    print ("ids: " ++ show (ids))
    let numericIds = filter (\s -> not (s == "x")) ids :: [String]
        nums = map (\s -> read s :: Int) numericIds :: [Int]
        p1 = part1 timestamp nums :: Int
    print ("part1 result: " ++ (show p1))
    let constraints = buildConstraints (zip [0..] ids)
        bignums = map (\s -> read s :: Int64) numericIds :: [Int64]
        p = product nums
    print constraints
    print p


