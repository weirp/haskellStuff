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

fibs2 :: [Integer]
fibs2 = map fst (iterate fibs2_helper2 (0,1))




data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a s) = a : streamToList s

instance Show a => Show (Stream a) where
  show = show . take 20 .streamToList


streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a s) = (Cons (f a)) (streamMap f s)


streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))


nats :: Stream Integer
nats = streamFromSeed (+ 1) 0


interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a as) (Cons b bs) = Cons a (Cons b (interleaveStreams as bs))

{-
interleaveStreams (streamFromSeed id 0) nats

[2,4 .. 20]
{-
ruler :: Stream Integer
-}
