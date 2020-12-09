module Util (count, xor, takeRight, dropRight, quicksort) where

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