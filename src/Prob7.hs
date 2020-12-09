module Prob7 (prob7) where

import Data.Map (Map)
import qualified Data.Map as Map
import System.IO
import Text.Regex.TDFA


-- Map[Color, Seq[(Int, Color)]]
readData :: String -> IO (Map String [(Int, String)])
readData fileName = do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    let linesInFile = lines contents
        _ = print (head linesInFile)
        rows = map parseRule linesInFile
        _ = hClose handle
    return (Map.fromList rows)


--shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
--dark olive bags contain 3 faded blue bags, 4 dotted black bags.
--vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
--faded blue bags contain no other bags.
parseRule :: String -> (String, [(Int, String)])
parseRule line =
    let regex = "\\`([a-z]+ [a-z]+) bags contain ([0-9]+) ([a-z]+ [a-z]+) bags? (, ([0-9]+) ([a-z]+ [a-z]+) bags)*\\.\\'"  
        (_, _, _, groups) = line =~ regex :: (String, String, String, [String])
        color = head groups
        

    in (color, [])

-- how many colors can, eventually, contain at least one shiny gold bag?
part1 :: Map String [(Int, String)] -> Int
part1 rules = -1


prob7 :: IO ()
prob7 = do
    rules <- readData "inputs/prob7.txt"
    print (head (Map.toList rules))
    let p1 = part1 rules
    print p1