module Golf where

import Data.List

{- Hopscotch
given a list, take every element, then every 2nd element, every third, etc
-}
skips :: [a] -> [[a]]
skips [] = [[]]
skips lst = map (skipsHelper lst) [1..(length lst)]


skipsHelper :: [a] -> Int -> [a]
skipsHelper [] _ = []
skipsHelper lst n
  | (length lst) >= n =
      (head (take 1 (drop (n - 1) lst))) : (skipsHelper (drop n lst) n)
  | otherwise = []
