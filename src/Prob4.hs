module Prob4 (prob4) where

import System.IO
import Data.Maybe
import Data.List
import Data.String.Utils
import Util
import Text.Regex.TDFA


data Passport = Passport { byr :: Maybe Int,
                            iyr :: Maybe Int,
                            eyr :: Maybe Int,
                            hgt :: Maybe String,
                            hcl :: Maybe String,
                            ecl :: Maybe String,
                            pid :: Maybe String,
                            cid :: Maybe String } deriving (Show)

passportDefault = Passport { byr = Nothing, iyr = Nothing, eyr = Nothing, hgt = Nothing, hcl = Nothing, ecl = Nothing, pid = Nothing, cid = Nothing }

readData :: String -> IO [String]
readData fileName = do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    let linesInFile = lines contents
        _ = print (head linesInFile)
        passportLines = reverse (flattenInput linesInFile)
        _ = print "hello"
        _ = print (head passportLines)
        passports = map parsePassport passportLines
        _ = hClose handle

    return passportLines


readDataPassports :: String -> IO [Passport]
readDataPassports fileName = do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    let linesInFile = lines contents
        _ = print (head linesInFile)
        passportLines = reverse (flattenInput linesInFile)
        _ = print "hello"
        _ = print (head passportLines)
        passports = map parsePassport passportLines
        _ = hClose handle

    return passports



flattenInput :: [String] -> [String]
flattenInput = foldl accf []


accf :: [String] -> String -> [String]
accf passportLines "" = "":passportLines
accf [] line = [line]
accf (p:passportLines) line = (p ++ " " ++ line):passportLines

parsePassport :: String -> Passport
parsePassport s =
    let byrRegex = "byr:([0-9]{4})"
        (_, _, _, byrGroups) = s =~ byrRegex :: (String, String, String, [String])
        byr = fmap (\s -> read s :: Int) (listToMaybe byrGroups)

        iyrRegex = "iyr:([0-9]{4})"
        (_, _, _, iyrGroups) = s =~ iyrRegex :: (String, String, String, [String])
        iyr = fmap (\s -> read s :: Int) (listToMaybe iyrGroups)

        eyrRegex = "eyr:([0-9]{4})"
        (_, _, _, eyrGroups) = s =~ eyrRegex :: (String, String, String, [String])
        eyr = fmap (\s -> read s :: Int) (listToMaybe eyrGroups)

        hgtRegex = "hgt:([0-9]{2,3}cm|in)"
        (_, _, _, hgtGroups) = s =~ hgtRegex :: (String, String, String, [String])
        hgt = listToMaybe hgtGroups

        hclRegex = "hcl:(#[0-9a-f]{6})"
        (_, _, _, hclGroups) = s =~ hclRegex :: (String, String, String, [String])
        hcl = listToMaybe hclGroups

        eclRegex = "ecl:(amb|blu|brn|gry|grn|hzl|oth)"
        (_, _, _, eclGroups) = s =~ eclRegex :: (String, String, String, [String])
        ecl = listToMaybe eclGroups

        pidRegex = "pid:([0-9]{9})"
        (_, _, _, pidGroups) = s =~ pidRegex :: (String, String, String, [String])
        pid = listToMaybe pidGroups

    in Passport {byr = byr, iyr = iyr, eyr = eyr, hgt = hgt, hcl = hcl, ecl = ecl, pid = pid, cid = Nothing}



isValidStr :: String -> Bool
isValidStr s = isInfixOf "byr:" s  && isInfixOf "iyr:" s && isInfixOf "eyr:" s && isInfixOf "hgt:" s && isInfixOf "hcl:" s && isInfixOf "ecl:" s && isInfixOf "pid:" s

part1 :: [String] -> Int
part1 = count isValidStr

-- ignore cid but all other fields must be present to consider a passport valid
-- byr, iyr, eyr, and hgt are all further validated values, but hcl,ecl, and pid are checked at the regex level
isValid :: Passport -> Bool
isValid p = isByrValid (byr p) && isIyrValid (iyr p) && isEyrValid (eyr p) && isHgtValid (hgt p) && isJust (hcl p) && isJust (ecl p) && isJust (pid p)

isByrValid :: Maybe Int -> Bool
isByrValid (Just byr) = 1920 <= byr && byr <= 2002
isByrValid Nothing = False

isIyrValid :: Maybe Int -> Bool
isIyrValid (Just iyr) = 2010 <= iyr && iyr <= 2020
isIyrValid Nothing = False

isEyrValid :: Maybe Int -> Bool
isEyrValid (Just eyr) = 2020 <= eyr && eyr <= 2030
isEyrValid Nothing = False

-- If cm, the number must be at least 150 and at most 193.
-- If in, the number must be at least 59 and at most 76
isHgtValid :: Maybe String -> Bool
isHgtValid Nothing = False
isHgtValid (Just hgt) =
    let
        suffix = takeRight 2 hgt
    in if suffix == "cm"
          then isHgtCmValid hgt
       else if suffix == "in"
              then isHgtInValid hgt
            else False

-- validate hgt in centimeters
isHgtCmValid :: String -> Bool
isHgtCmValid hgt =
    let suffix = takeRight 2 hgt
        nums = read (dropRight 2 hgt) :: Int
     in suffix == "cm" && 150 <= nums && nums <= 193


-- validate hgt in inches
isHgtInValid :: String -> Bool
isHgtInValid hgt =
    let suffix = takeRight 2 hgt
        nums = read (dropRight 2 hgt) :: Int
     in suffix == "in" && 59 <= nums && nums <= 76


part2 :: [Passport] -> IO Int
part2 passports = do
    let validPassports = filter isValid passports
        cnt = length validPassports
    print validPassports

    return cnt

--debug :: Passport -> IO ()
--debug passport = do
--    print passport
--    let v = isValid passport
--    print v

debugPassports :: [Passport] -> IO ()
debugPassports [] = do
    return ()
debugPassports (x:xs) = do
    print x
    let v = isValid x
    print v
    debugPassports xs

-- 110 too high
prob4 :: IO ()
prob4 = do
    rows <- readData "inputs/prob4.txt"
    print (head rows)
    let examplePassport = passportDefault
--    print examplePassport
--    print (isValid examplePassport)
    let examplePassport2 = passportDefault { byr = Just 1, iyr = Just 2, eyr = Just 3, hgt = Just "4", hcl = Just "5", ecl = Just "6", pid = Just "7"}
--    print examplePassport2
    print (isValid examplePassport2)
    let p1 = part1 rows
    print p1
    part2Rows <- readDataPassports "inputs/prob4.txt"
    p2 <- part2 part2Rows
    v  <- debugPassports part2Rows
    print p2
