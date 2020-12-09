module Prob3 (prob3) where

import System.IO
import Util

readData :: String -> IO [String]
readData fileName = do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    let linesInFile = lines contents
        _ = hClose handle
    return linesInFile


checkTrees :: [String] -> Int -> Int -> Int
checkTrees grid rowStep colStep =
    let
        numRows = length grid
        numCols = length $ head grid
        rows = [0,rowStep..(numRows - 1)]
        cols = [0,colStep..]
        pairs = zip rows cols
        cnt = count p pairs
            where p (r, c) = ((grid !! r) !! (mod c numCols)) == '#'

    in cnt


part1 :: [String] -> Int
part1 grid = checkTrees grid 1 3

part2 :: [String] -> Int
part2 grid =
    let
        steps = [(1,1), (1,3), (1,5), (1,7), (2,1)]
        trees = map f steps
            where f (rowStep, colStep) = checkTrees grid rowStep colStep 
        
    in product trees


prob3 :: IO ()
prob3 = do
    rows <- readData "inputs/prob3.txt"
    print (head rows)
    let p1 = part1 rows
    print p1
    let p2 = part2 rows
    print p2