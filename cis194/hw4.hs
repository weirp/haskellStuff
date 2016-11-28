module Homework4 where

{-
Exercise 1: Wholemeal programming
Reimplement each of the following functions in a more idiomatic
Haskell style. Use wholemeal programming practices, breaking each
function into a pipeline of incremental transformations to an entire
data structure. Name your functions fun1’ and fun2’ respectively.
1. fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
| even x = (x - 2) * fun1 xs
| otherwise = fun1 xs
2. fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n ‘div‘ 2)
| otherwise = fun2 (3 * n + 1)
Hint: For this problem you may wish to use the functions iterate
and takeWhile. Look them up in the Prelude documentation to see
what they do.
-}
fun1' :: [Integer] -> Integer
fun1' = product . (map (\x -> x - 2)) . filter even


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

{-
  We are only accumulating even inputs.
  generate even n -> (n `div` 2)
           otherwise (3 * n + 1)
  terminate when n = 1

-}


fun2' :: Integer -> Integer
fun2' n = sum $ filter even $ (takeWhile (> 1)
                  (iterate fn2_helper n))


fn2_helper :: Integer -> Integer
fn2_helper n
  | even n = n `div` 2
  | otherwise = 3 * n + 1
