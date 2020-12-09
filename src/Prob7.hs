module Prob7 (prob7) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.String.Utils
import System.IO
import Text.Regex.TDFA

-- Map[Color, Seq[(Int, Color)]]
readData :: String -> IO (Map String [(Int, String)])
readData fileName = do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    let linesInFile = lines contents
        trimmed = fmap rstrip linesInFile :: [String]
        rules = map parseRule trimmed
        _ = hClose handle
    print ("num trimmed lines: " ++ (show (length trimmed)))
    print ("first line in file: " ++ (head trimmed))
    return (Map.fromList rules)


--shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
--dark olive bags contain 3 faded blue bags, 4 dotted black bags.
--vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
--faded blue bags contain no other bags.
parseRule :: String -> (String, [(Int, String)])
parseRule line =
    let regex = "\\`([a-z]+ [a-z]+) bags contain (.*)\\.\\'"
        (_, _, _, groups) = line =~ regex :: (String, String, String, [String])
        fromColor = head groups
        innerBagsStr = groups !! 1

    in (fromColor, (parseContainedBags innerBagsStr))

parseContainedBags :: String -> [(Int, String)]
parseContainedBags "no other bags" = []
parseContainedBags s = fmap parseContainedBag (split ", " s)

-- 3 faded green bags
parseContainedBag :: String -> (Int, String)
parseContainedBag s =
    let regex = "\\`([0-9]+) ([a-z]+ [a-z]+) bags?\\'"
        (_, _, _, groups) = s =~ regex :: (String, String, String, [String])
        amt = read (head groups) :: Int
        color = groups !! 1
    in (amt, color)




-- how many colors can, eventually, contain at least one shiny gold bag?
part1 :: Map String [(Int, String)] -> Int
part1 rules = -1


prob7 :: IO ()
prob7 = do
    let s1 = "bright orange bags contain 5 dim bronze bags."
        r1 = parseRule s1
    print s1
    print (r1)
    let s2 = "bright gold bags contain no other bags."
        r2 = parseRule s2
    print s2
    print (r2)
    let s3 = "vibrant maroon bags contain 3 faded green bags, 3 bright chartreuse bags, 4 clear orange bags, 2 dim tomato bags."
        r3 = parseRule s3
    print s3
    print (r3)

    rules <- readData "inputs/prob7.txt"
    print ("num rules: " ++ (show (length rules)))
    let p1 = part1 rules
    print ("part1 result: " ++ (show p1))
