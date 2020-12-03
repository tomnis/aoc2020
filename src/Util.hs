module Util (count, xor) where

count :: (a -> Bool) -> [a] -> Int
count p xs = length (filter p xs)

xor :: Bool -> Bool -> Bool
xor a b = a /= b