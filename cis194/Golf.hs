module Golf where

import Data.List

{- Hopscotch
given a list, take every element, then every 2nd element, every third, etc
-}
skips :: [a] -> [[a]]
skips [] = []
skips lst = map (skipsHelper lst) [1..(length lst)]


skipsHelper :: [a] -> Int -> [a]
skipsHelper [] _ = []
skipsHelper lst n
  | (length lst) >= n =
      (head (take 1 (drop (n - 1) lst))) : (skipsHelper (drop n lst) n)
  | otherwise = []

{-
Exercise 2 Local maxima
A local maximum of a list is an element of the list which is strictly
greater than both the elements immediately before and after it. For
example, in the list [2,3,4,1,5], the only local maximum is 4, since
it is greater than the elements immediately before and after it (3 and
1). 5 is not a local maximum since there is no element that comes
after it.
Write a function
localMaxima :: [Integer] -> [Integer]
which finds all the local maxima in the input list and returns them in
order. For example:
localMaxima [2,9,5,6,1] == [9,6]
localMaxima [2,3,4,1,5] == [4]
localMaxima [1,2,3,4,5] == []
-}
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima lst@(x:y:z:_)
  | (y > x) && (y > z) = y : localMaxima (tail lst)
  | otherwise = localMaxima (tail lst)
localMaxima lst = localMaxima (tail lst)
