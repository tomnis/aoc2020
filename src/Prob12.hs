module Prob12 (prob12) where

import Data.Maybe
import Data.String.Utils
import System.IO
import Util

data Action = MoveNorth | MoveSouth | MoveEast | MoveWest | TurnLeft | TurnRight | MoveForward
    deriving Show
data FacingDirection = FacingNorth | FacingSouth | FacingEast | FacingWest
    deriving Show
-- x, y
type Position = (Int, Int)
type Instruction = (Action, Int)
type Ship = (FacingDirection, Position)

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
parseChar 'N' = MoveNorth
parseChar 'S' = MoveSouth
parseChar 'E' = MoveEast
parseChar 'W' = MoveWest
parseChar 'L' = TurnLeft
parseChar 'R' = TurnRight
parseChar 'F' = MoveForward
parseChar other = error ("not supported: " ++ show other)

parseLine :: String -> Instruction
parseLine (cmd:arg) = (parseChar cmd, read arg :: Int)

-- assume distance from origin
manhattanDistance :: Int -> Int -> Int
manhattanDistance x y = (abs x) + (abs y)

runInstruction :: Ship -> Instruction -> Ship
runInstruction (dir, (x, y)) (MoveNorth, arg) = (dir, (x, y + arg))
runInstruction (dir, (x, y)) (MoveSouth, arg) = (dir, (x, y - arg))
runInstruction (dir, (x, y)) (MoveEast, arg) = (dir, (x + arg, y))
runInstruction (dir, (x, y)) (MoveWest, arg) = (dir, (x - arg, y))
runInstruction (FacingNorth, (x, y)) (MoveForward, arg) = (FacingNorth, (x, y + arg))
runInstruction (FacingSouth, (x, y)) (MoveForward, arg) = (FacingSouth, (x, y - arg))
runInstruction (FacingEast, (x, y)) (MoveForward, arg) = (FacingEast, (x + arg, y))
runInstruction (FacingWest, (x, y)) (MoveForward, arg) = (FacingWest, (x - arg, y))
runInstruction (FacingNorth, pos) (TurnLeft, 90) = (FacingWest, pos)
runInstruction (FacingNorth, pos) (TurnLeft, n) = runInstruction (FacingWest, pos) (TurnLeft, n - 90)
runInstruction (FacingNorth, pos) (TurnRight, 90) = (FacingEast, pos)
runInstruction (FacingNorth, pos) (TurnRight, n) = runInstruction (FacingEast, pos) (TurnRight, n - 90)
runInstruction (FacingEast, pos) (TurnLeft, 90) = (FacingNorth, pos)
runInstruction (FacingEast, pos) (TurnLeft, n) = runInstruction (FacingNorth, pos) (TurnLeft, n - 90)
runInstruction (FacingEast, pos) (TurnRight, 90) = (FacingSouth, pos)
runInstruction (FacingEast, pos) (TurnRight, n) = runInstruction (FacingSouth, pos) (TurnRight, n - 90)
runInstruction (FacingSouth, pos) (TurnLeft, 90) = (FacingEast, pos)
runInstruction (FacingSouth, pos) (TurnLeft, n) = runInstruction (FacingEast, pos) (TurnLeft, n - 90)
runInstruction (FacingSouth, pos) (TurnRight, 90) = (FacingWest, pos)
runInstruction (FacingSouth, pos) (TurnRight, n) = runInstruction (FacingWest, pos) (TurnRight, n - 90)
runInstruction (FacingWest, pos) (TurnLeft, 90) = (FacingSouth, pos)
runInstruction (FacingWest, pos) (TurnLeft, n) = runInstruction (FacingSouth, pos) (TurnLeft, n - 90)
runInstruction (FacingWest, pos) (TurnRight, 90) = (FacingNorth, pos)
runInstruction (FacingWest, pos) (TurnRight, n) = runInstruction (FacingNorth, pos) (TurnRight, n - 90)

run :: Ship -> [Instruction] -> Ship
run ship [] = ship
run ship (i:is) = run (runInstruction ship i) is

part1 :: [Instruction] -> Int
part1 is =
    let (_, (x, y))= run (FacingEast, (0, 0)) is
        in manhattanDistance x y

-------------------------------------------------------------------
-------------------------------------------------------------------
-------------------------------------------------------------------
-- relative to ship position
type Waypoint = (Int, Int)

-- we no longer care about ship facing direction
runInstruction2 :: (Position, Waypoint) -> Instruction -> (Position, Waypoint)
-- move the waypoint
runInstruction2 (ship, (wx, wy)) (MoveNorth, arg) = (ship, (wx, wy + arg))
runInstruction2 (ship, (wx, wy)) (MoveSouth, arg) = (ship, (wx, wy - arg))
runInstruction2 (ship, (wx, wy)) (MoveEast, arg) = (ship, (wx + arg, wy))
runInstruction2 (ship, (wx, wy)) (MoveWest, arg) = (ship, (wx - arg, wy))
-- rotate the waypoint about the ship
runInstruction2 (ship, (wx, wy)) (TurnLeft, 90) = (ship, (wy * (-1), wx))
runInstruction2 (ship, (wx, wy)) (TurnLeft, n) = runInstruction2 (ship, (wy * (-1), wx)) (TurnLeft, n - 90)
runInstruction2 (ship, (wx, wy)) (TurnRight, 90) = (ship, (wy, wx * (-1)))
runInstruction2 (ship, (wx, wy)) (TurnRight, n) = runInstruction2 (ship, (wy, wx * (-1))) (TurnRight, n - 90)
runInstruction2 ((sx, sy), (wx, wy)) (MoveForward, n) = ((sx + (wx * n), sy + (wy * n)), (wx, wy))

run2 :: (Position, Waypoint) -> [Instruction] -> Position
run2 (ship, _) [] = ship
run2 (ship, waypoint) (i:is) = run2 (runInstruction2 (ship, waypoint) i) is

part2 :: [Instruction] -> Int
part2 is =
    let (x, y) = run2 ((0, 0), (10, 1)) is
        in manhattanDistance x y

prob12 :: IO ()
prob12 = do
    print "hi"
    instructions <- readData "inputs/prob12.txt"
    print ("num instructions: " ++ show (length instructions))
    print ("first instruction: " ++ show (head instructions))
    let p1 = part1 instructions :: Int
    print ("part1 result: " ++ (show p1))
    let p2 = part2 instructions :: Int
    print ("part2 result: " ++ (show p2))

