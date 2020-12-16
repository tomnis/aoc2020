module Prob12 (prob12) where

import Data.Maybe
import Data.String.Utils
import System.IO
import Util

data Action = North | South | East | West | Left | Right | Forward
    deriving Show
data Direction = FacingNorth | FacingSouth | FacingEast | FacingWest
    deriving Show
-- x, y
type Position = (Int, Int)
type Instruction = (Action, Int)
type Ship = (Direction, Position)

readData :: String -> IO [Instruction]
readData fileName = do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    let linesInFile = lines contents
        trimmed = fmap rstrip linesInFile :: [String]
        instructions = fmap parseLine trimmed :: [Instruction]
        _ = hClose handle
    print ("num trimmed lines: " ++ show (length trimmed))
    print ("first line in file: " ++ head trimmed)
    return instructions

parseChar :: Char -> Action
parseChar 'N' = North
parseChar 'S' = South
parseChar 'E' = East
parseChar 'W' = West
parseChar 'L' = Prob12.Left
parseChar 'R' = Prob12.Right
parseChar 'F' = Forward
parseChar other = error ("not supported: " ++ show other)

parseLine :: String -> Instruction
parseLine (cmd:arg) = (parseChar cmd, read arg :: Int)



runInstruction :: Ship -> Instruction -> Ship
runInstruction (dir, (x, y)) (North, arg) = (dir, (x, y + arg))
runInstruction (dir, (x, y)) (South, arg) = (dir, (x, y - arg))
runInstruction (dir, (x, y)) (East, arg) = (dir, (x + arg, y))
runInstruction (dir, (x, y)) (West, arg) = (dir, (x - arg, y))
runInstruction ship _ = ship
-- TODO turning


prob12 :: IO ()
prob12 = do
    print "hi"
    instructions <- readData "inputs/prob12.txt"
    print ("num instructions: " ++ show (length instructions))
    print ("first instruction: " ++ show (head instructions))

