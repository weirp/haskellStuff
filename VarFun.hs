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
:type 32
:t 'H'
:t (3 < 5)
:module Data.Char
:m Data.Char
chr 97
ord 'c'

{- up to https://en.wikibooks.org/wiki/Haskell/Lists_and_tuples -}
