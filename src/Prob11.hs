module Prob11 (prob11 )where

import Data.Maybe
import Data.String.Utils
import System.IO
import Util

type Grid = [String]
-- row, col
type Cell = (Int, Int)

-- floor '.'
-- empty 'L'
-- occupied '#'

readData :: String -> IO Grid
readData fileName = do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    let linesInFile = lines contents
        trimmed = fmap rstrip linesInFile :: [String]
        _ = hClose handle
    print ("num trimmed lines: " ++ show (length trimmed))
    print ("first line in file: " ++ head trimmed)
    return trimmed


-- grid, row, col -> count
numOccupiedNeighbors :: Grid -> Cell -> Int
numOccupiedNeighbors grid (row, col)  =
    let neighbors = [(row - 1, col - 1), (row - 1, col), (row - 1, col + 1),
                     (row, col - 1),                     (row, col + 1),
                     (row + 1, col - 1), (row + 1, col), (row + 1, col + 1)] :: [Cell]
    in count (isOccupied grid) neighbors

isOccupied :: Grid -> Cell -> Bool
isOccupied grid cell = (lookupSeat grid cell) == (Just '#')

lookupSeat :: Grid -> Cell -> Maybe Char
lookupSeat grid (row, col) = do
    maybeRow <- safeAccess grid row :: Maybe String
    safeAccess maybeRow col :: Maybe Char

numOccupiedSeats :: Grid -> Int
numOccupiedSeats grid = sum (map (count (== '#')) grid)



stepCell :: Grid -> Cell -> Char -> Char
-- If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied
stepCell grid cell 'L'
    | numOccupiedNeighbors grid cell == 0 = '#'
    | otherwise = 'L'
-- If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
stepCell grid cell '#'
    | numOccupiedNeighbors grid cell >= 4 = 'L'
    | otherwise = '#'
stepCell _ _ '.' = '.'
stepCell _ _ other = error ("wrong cell: " ++ (show other))


stepRow :: Grid -> (Int, String) -> String
stepRow grid (idx, row) =
    let pairs = zip [0..] row :: [(Int, Char)]
    in map (\(col, char) -> stepCell grid (idx, col) char) pairs

-- otherwise state does not change
step :: Grid -> Grid
step grid =
    let pairs = zip [0..] grid :: [(Int, String)]
    in map (\(rowIdx, row) -> stepRow grid (rowIdx, row)) pairs

part1 :: Grid -> Int
part1 oldGrid =
    let newGrid = step oldGrid
    in if oldGrid == newGrid
            then numOccupiedSeats newGrid
       else part1 newGrid


-------------------------------------------
-------------------------------------------
-------------------------------------------

-- scan starting in the specified direction
scanUntilSeat :: Grid -> Cell -> (Int, Int) -> Maybe Char
scanUntilSeat grid start delta =
    let newCell = add2 start delta
        maybeCh = lookupSeat grid newCell
    in checkSeat grid newCell delta maybeCh

checkSeat :: Grid -> Cell -> (Int, Int) -> Maybe Char -> Maybe Char
checkSeat _ _ _ (Just 'L') = Just 'L'
checkSeat _ _ _ (Just '#') = Just '#'
-- floor -- keep scanning
checkSeat grid cur delta (Just '.') = scanUntilSeat grid cur delta
-- went past the edge
checkSeat _ _ _ Nothing = Nothing
checkSeat _ _ _ ch = error ("wrong cell: " ++ (show ch))


--  the first seat they can see in each of those eight directions
numOccupiedNeighbors2 :: Grid -> Cell -> Int
numOccupiedNeighbors2 grid (row, col) =
    let directions = [(-1, -1), (-1, 0), (-1, 1),
                      (0, -1),           (0, 1),
                      (1, -1), (1, 0), (1, 1)]
    in count (\delta -> scanUntilSeat grid (row, col) delta == Just '#') directions


stepCell2 :: Grid -> Cell -> Char -> Char
-- If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied
stepCell2 grid cell 'L'
    | numOccupiedNeighbors2 grid cell == 0 = '#'
    | otherwise = 'L'
-- If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
stepCell2 grid cell '#'
    | numOccupiedNeighbors2 grid cell >= 5 = 'L'
    | otherwise = '#'
stepCell2 _ _ '.' = '.'
stepCell2 _ _ other = error ("wrong cell: " ++ (show other))


stepRow2 :: Grid -> (Int, String) -> String
stepRow2 grid (idx, row) =
    let pairs = zip [0..] row :: [(Int, Char)]
    in map (\(col, cell) -> stepCell2 grid (idx, col) cell) pairs


step2 :: Grid -> Grid
step2 grid =
    let pairs = zip [0..] grid :: [(Int, String)]
    in map (stepRow2 grid) pairs

part2 :: Grid -> Int
part2 oldGrid =
    let newGrid = step2 oldGrid
    in if oldGrid == newGrid
            then numOccupiedSeats newGrid
       else part2 newGrid


prob11 :: IO ()
prob11 = do
    print "hi"
    grid <- readData "inputs/prob11.txt"
    print ("num grid rows: " ++ (show (length grid)))
    print ("grid row 1: " ++ (show (head grid)))
    let p1 = part1 grid :: Int
    print ("part1 result: " ++ (show p1))
    -- 5825 too high
    let p2 = part2 grid :: Int
    print ("part2 result: " ++ (show p2))
