module Prob8 (prob8) where

import Data.List
import Data.Maybe
import Data.String.Utils
import qualified Data.HashSet as HashSet
import System.IO
import Util

-- (LineNumber, Operation, Arg)
--readData :: String -> IO [String]
readData :: String -> IO [(Int, String, Int)]
readData fileName = do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    let linesInFile = lines contents
        trimmed = fmap rstrip linesInFile :: [String]
        args = fmap (split " ") trimmed :: [[String]]
        enumerated = zip [0..] trimmed
        _ = hClose
    return (fmap (\(i,s) -> (i, (take 3 s), (readSignedInt (drop 4 s)))) enumerated)


-- program -> seen lines -> acc -> ip -> curLine
runUntilLoop :: [(Int, String, Int)] -> HashSet.HashSet Int -> Int -> Int -> (Int, String, Int) -> Int
runUntilLoop program visited acc ip currentInstruction
    | elem ip visited = acc
    | otherwise = step program visited acc ip currentInstruction


step :: [(Int, String, Int)] -> HashSet.HashSet Int -> Int -> Int -> (Int, String, Int) -> Int
step program visited acc ip (line, "nop", arg) = runUntilLoop program (HashSet.insert line visited) acc (ip + 1) (program !! (ip + 1))
step program visited acc ip (line, "acc", arg) = runUntilLoop program (HashSet.insert line visited) (acc + arg) (ip + 1) (program !! (ip + 1))
step program visited acc ip (line, "jmp", arg) = runUntilLoop program (HashSet.insert line visited) acc (ip + arg) (program !! (ip + arg))
step _ _ _ _ _ = error "unsupported operation"


-- acc starts at 0
-- Immediately before any instruction is executed a second time, what value is in the accumulator?
part1 :: [(Int, String, Int)] -> Int
part1 program = runUntilLoop program HashSet.empty 0 0 (head program)


run :: [(Int, String, Int)] -> Maybe Int
run program = terminatesNormally program HashSet.empty 0 0


-- program -> seen lines -> acc -> ip
terminatesNormally :: [(Int, String, Int)] -> HashSet.HashSet Int -> Int -> Int -> Maybe Int
terminatesNormally program visited acc ip
    | elem ip visited = Nothing
    | ip >= (length program) = Just acc
    | otherwise = step2 program visited acc ip (program !! ip)

step2 :: [(Int, String, Int)] -> HashSet.HashSet Int -> Int -> Int -> (Int, String, Int) -> Maybe Int
step2 program visited acc ip (line, "nop", arg) = terminatesNormally program (HashSet.insert line visited) acc (ip + 1)
step2 program visited acc ip (line, "acc", arg) = terminatesNormally program (HashSet.insert line visited) (acc + arg) (ip + 1)
step2 program visited acc ip (line, "jmp", arg) = terminatesNormally program (HashSet.insert line visited) acc (ip + arg)
step2 _ _ _ _ _ = error "unsupported operation"


swap :: (Int, String, Int) -> (Int, String, Int)
swap (idx, "nop", arg) = (idx, "jmp", arg)
swap (idx, "jmp", arg) = (idx, "nop", arg)
swap _ = error "should not swap for acc"

modifyProgram :: [(Int, String, Int)] -> Int -> [(Int, String, Int)]
modifyProgram program idx =
    let line = program !! idx
        newLine = swap line
    in (take idx program) ++ (newLine : (drop (idx + 1) program))

unbox :: (Maybe (Maybe Int)) -> Int
unbox (Just (Just x)) = x
unbox _ = -1

-- Fix the program so that it terminates normally by changing exactly one jmp (to nop) or nop (to jmp).
-- What is the value of the accumulator after the program terminates?
part2 :: [(Int, String, Int)] -> Int
part2 program =
    let linesToModify = filter (\(_, op, _) -> op == "jmp" || op == "nop") program :: [(Int, String, Int)]
        indices = fmap (\(ip, _, _) -> ip) linesToModify :: [Int]
    -- map indices to create new modified programs
        newPrograms = map (\idx -> modifyProgram program idx) indices :: [[(Int, String, Int)]]
    -- then run each modified program to see which terminates normally
        fixedProgramResult = find (\x -> isJust x) (map run newPrograms)
    in unbox fixedProgramResult

prob8 :: IO ()
prob8 = do
    program <- readData "inputs/prob8.txt"
    print (head program)
    print (program !! 7)
    let p1 = part1 program
    print ("part1 result: " ++ (show p1))
    let p2 = part2 program
    print ("part2 result: " ++ (show p2))
