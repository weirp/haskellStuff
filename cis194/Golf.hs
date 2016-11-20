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


{-
Exercise 3 Histogram
For this task, write a function
histogram :: [Integer] -> String
which takes as input a list of Integers between 0 and 9 (inclusive),
and outputs a vertical histogram showing how many of each number
were in the input list. You may assume that the input list does not
contain any numbers less than zero or greater than 9 (that is, it does
not matter what your function does if the input does contain such
numbers). Your output must exactly match the output shown in the
examples below.
cis 194: homework 3 4
histogram [1,1,1,5] ==
*
*
* *
==========
0123456789
histogram [1,4,5,4,6,6,3,4,2,4,9] ==
*
*
* *
****** *
==========
0123456789
Important note: If you type something like histogram [3,5] at
the ghci prompt, you should see something like this:
" * * \n==========\n0123456789\n"
This is a textual representation of the String output, including \n
escape sequences to indicate newline characters. To actually visualize
the histogram as in the examples above, use putStr, for example,
putStr (histogram [3,5]).
-}


histogram :: [Integer] -> String
histogram [] = ""
histogram lst =
  let m = map (\x -> head x, length x) $ group $ sort lst
  map (\x -> let l = lookup x m
             case l of
               Nothing = " "
               Just x) [0..9]
