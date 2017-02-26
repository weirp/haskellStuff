module Cipher where

import Data.Char


caeser :: Int -> String -> String
caeser = undefined

unCaeser :: Int -> String -> String
unCaeser = undefined


myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = if x == False then False else myAnd xs


myAnd2 :: [Bool] -> Bool
myAnd2 [] = True
myAnd2 (x:xs) = x && myAnd2 xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = myOr $ map f xs

myElem :: Eq a => a -> [a] -> Bool
myElem c xs = myAny (== c) xs

myReverse :: [a] -> [a]
myReverse [y] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f a = squish $ map f a

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f [] = undefined
myMaximumBy f (x:[]) = x
myMaximumBy f (x:xs) = if f x y == GT then x else y
  where y = (myMaximumBy f xs)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f [] = undefined
myMinimumBy f (x:[]) = x
myMinimumBy f (x:xs) = if f x y == LT then x else y
  where y = (myMaximumBy f xs)


myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
