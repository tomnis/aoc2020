module Util (count, xor, takeRight, dropRight, quicksort, readSignedInt, slidingWindows, safeAccess, unbox, add2) where

import Data.List
import Data.Maybe

count :: (a -> Bool) -> [a] -> Int
count p xs = length (filter p xs)

xor :: Bool -> Bool -> Bool
xor a b = a /= b

takeRight :: Int -> String -> String
takeRight n = reverse . take n . reverse

dropRight :: Int -> String -> String
dropRight n = reverse . drop n . reverse

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted


readSignedInt :: String -> Int
readSignedInt ('+':num) = read num :: Int
readSignedInt num = read num :: Int


slidingWindows :: Int -> [a] -> [[a]]
slidingWindows n xs = filter (\ns -> (length ns) == n) (map (take n) (tails xs))

safeAccess :: [a] -> Int -> Maybe a
safeAccess xs i
    | (i >=0) && (i < (length xs)) = Just (xs !! i)
    | otherwise = Nothing

unbox :: a -> Maybe a -> a
unbox _ (Just m) = m
unbox dflt Nothing = dflt


add2 :: (Int, Int) -> (Int, Int) -> (Int, Int)
add2 (a,b) (c,d) = (a + c, b + d)