{-# LANGUAGE NamedFieldPuns #-}

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

mySignum2 x
  | x < 0    = -1
  | x > 0    = 1
  |otherwise = 0

fst' :: (a, b) -> a
fst' (x, _) = x

{- https://en.wikibooks.org/wiki/Haskell/Building_vocabulary -}

revWords :: String -> String
revWords input = (unwords . reverse . words) input


contrivedMap :: ([a] -> a -> b) -> [a] -> [b]
contrivedMap f [] = []
contrivedMap f list@(x:xs) = f list x : contrivedMap f xs


data Foo2 = Bar2 | Baz2 {bazNumber::Int, bazName::String}

h2 :: Foo2 -> Int
h2 Baz2 {bazName=name} = length name
h2 Bar2 {} = 0

x2 = Baz2 1 "Haskell"
y2 = Baz2 {bazName = "Curry", bazNumber = 2}


quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x : xs) = (quickSort less) ++ (x : equal) ++ (quickSort more)
  where
    less = filter (< x) xs
    more = filter (> x) xs
    equal = filter (== x) xs

quickSort' :: (Ord a) => (a -> a -> Ordering) -> [a] -> [a]
quickSort' _ [] = []

quickSort' c (x : xs) = (quickSort' c less) ++ ( x : equal) ++ (quickSort' c more)
  where
    less  = filter (\y -> y `c` x == LT) xs
    equal = filter (\y -> y `c` x == EQ) xs
    more  = filter (\y -> y `c` x == GT) xs

{- compare fns -}
{- usual = compare
descending x y = compare y x
-}

myInits :: [a] -> [[a]]
myInits = map reverse . scanl (flip (:)) []

{- https://en.wikibooks.org/wiki/Haskell/Using_GHCi_effectively -}

data Month = January | February | March | April | May | June | July
           | August | September | October | November | December

data Configuration = Configuration
  { username      :: String
  , localHost     :: String
  , remoteHost    :: String
  , isGuest       :: Bool
  , isSuperUser   :: Bool
  , currentDir    :: String
  , homeDir       :: String
  , timeConnected :: Integer
  }

{-https://en.wikibooks.org/wiki/Haskell/Other_data_structures>-}

data Weird a b
  = First a
  | Second b
  | Third [(a,b)]
  | Fourth (Weird a b)


weirdMap :: (a -> c) -> (b -> d) -> Weird a b -> Weird c d
weirdMap fa fb = g
  where
    g (First x)  = First (fa x)
    g (Second y) = Second (fb y)
    g (Third z)  = Third [ (fa x, fb y) | (x,y) <- z ]
    g (Fourth w) = Fourth (g w)

weirdFold :: (a -> c) -> (b -> c) -> ([(a,b)] -> c) -> (c -> c) -> Weird a b -> c
weirdFold f1 f2 f3 f4 = g
  where
    g (First x)  = f1 x
    g (Second y) = f2 y
    g (Third z)  = f3 z
    g (Fourth w) = f4 (g w)

{- revisit other data structures -}


{- Understanding monads -}
{-
>>= is bind

return :: a -> m a
(>>=) :: m a -> (a -> m b) -> m b

-}
