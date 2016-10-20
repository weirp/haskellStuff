r = 5.0
area r = pi * r ^ 2

absolute_mine x
  | x < 0     = 0 - x
  | otherwise = x

{- use :reload to reload the file -}

numOfRealSolutions a b c
  | disc > 0   = 2
  | disc == 0  = 1
  | otherwise  = 0
    where
      disc = b^2 - 4*a*c

{- up to https://en.wikibooks.org/wiki/Haskell/Type_basics -}
{- :t 32
:t 'H'
:t (3 < 5)
:module Data.Char
:m Data.Char
chr 97
ord 'c'
-}
{- up to https://en.wikibooks.org/wiki/Haskell/Lists_and_tuples -}

x = 2

{- becuse length returns an Integral, and / requires a Fractional -}
yyy = 4 / fromIntegral (length [1,2,3])

{-https://en.wikibooks.org/wiki/Haskell/Next_steps-}

mySignum x =
  if x < 0
     then -1
     else if x > 0
             then 1
             else 0

mySignum2 x =
  | x < 0    = -1
  | x > 0    = 1
  |otherwise = 0

fst' :: (a, b) -> a
fst' (x, _) = x

{- https://en.wikibooks.org/wiki/Haskell/Building_vocabulary -}

revWords :: String -> String
revWords input = (unwords . reverse . words) input
