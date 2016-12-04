module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (fib (n - 1)) + (fib (n - 2))

fibs1 :: [Integer]
fibs1 = map fib [0..]

{-
fibs2 :: [Integer]

use iterate to generate list of fibs. then head $ take n

this list could be a list of pairs... take the minimum of the 2??

take 10 (iterate fibs2_helper2 (0,1))
-}

fibs2_helper1 :: Integer -> Integer -> Integer
fibs2_helper1 n1 n2 = n1 + n2

fibs2_helper2 :: (Integer, Integer) -> (Integer, Integer)
fibs2_helper2 (n1, n2) = (maximum [n1, n2], n1 + n2)
