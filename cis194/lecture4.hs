module Lecture4 where

greaterThan100 :: [Integer] -> [Integer]
greaterThan100 xs = filter gt100 xs

gt100 :: Integer -> Bool
gt100 x = x > 100


greaterThan100_3 :: [Integer] -> [Integer]
greaterThan100_3 xs = filter (>100) xs


myTest :: [Integer] -> Bool
myTest xs = even (length (greaterThan100 xs))

myTest' :: [Integer] -> Bool
myTest' = even . length . greaterThan100
