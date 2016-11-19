module Golf where

import Data.List

{- Hopscotch
given a list, take every element, then every 2nd element, every third, etc
-}
skips :: [a] -> [[a]]
skips [] = [[]]
skips all@(x:xs) = map (skipsHelper all) [1..(length all)]


skipsHelper :: [a] -> Int -> [a]
skipsHelper [] _ = []
skipsHelper all@(x:xs) n
  | (length all) >= n = (head (take 1 (drop (n - 1) all))) : (skipsHelper (drop n all) n)
  | otherwise = []
