module Fib where

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)


type Numerator = Integer
type Denominator = Integer
type Quotient = Integer

dividedBy1 :: Numerator -> Denominator -> Quotient
dividedBy1 = div

{-
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)
-}

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy = flip cattyConny

appedCatty = cattyConny "woops"
frappe = flippy "haha"

summery :: (Eq a, Num a) => a -> a
summery n = go n
  where go n
          | n == 0 = 0
          | otherwise = n + go (n - 1)

recSum :: (Integral a) => a -> a -> a
recSum 0 _ = 0
recSum _ 0 = 0
recSum 1 n = n
recSum n 1 = n
recSum m n = m + recSum m (n - 1)

data DividedResult
  = Result Integer
  | DividedByZero
  deriving (Show)


divBy :: Integral a => a -> a -> DividedResult
divBy _ 0 = DividedByZero
divBy num den = Result $ toInteger $ div num den


-- McCarthy91
mc91 :: Integral a => a -> a
mc91 n = go n
  where go n
          | n > 100 = n - 10
          | otherwise = 91
