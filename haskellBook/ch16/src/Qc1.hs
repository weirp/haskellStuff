{-# LANGUAGE ViewPatterns #-}
module Qc1 where

import Data.Char
import Test.QuickCheck
import Test.QuickCheck.Function

data WhoCares' a
  = ItDoesnt'
  | Matter' a
  | WhatThisIsCalled'
  deriving (Eq, Show)

instance Functor WhoCares' where
  fmap _ ItDoesnt' = ItDoesnt'
  fmap _ WhatThisIsCalled' = WhatThisIsCalled'
  fmap f (Matter' a) = Matter' (f a)

functorIdentityX :: (Functor f, Eq (f a)) =>
                    f a
                 -> Bool
functorIdentityX f = fmap id f == f

functorComposeX :: (Eq (f c), Functor f) =>
                   f a
                -> Fun a b
                -> Fun b c
                -> Bool

functorComposeX x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

-- from https://ocharles.org.uk/blog/posts/2012-12-08-24-days-of-hackage.html
absAverage :: [Double] -> Double
absAverage ds = sum (map abs ds) / fromIntegral (length ds)


-- whatever, from https://wiki.haskell.org/Introduction_to_QuickCheck
--  think the QuickCheck2 version is reqd https://wiki.haskell.org/Introduction_to_QuickCheck2
getList :: IO [Char]
getList = fmap take5 getContents

take5 :: [Char] -> [Char]
take5 = take 5 . filter (`elem` ['a'..'e'])



main :: IO ()
main = do
  quickCheck $ \x -> functorIdentityX (x :: [Int])
  quickCheck $ \x -> length x > 1 ==> absAverage x >= 0


  quickCheck (\s -> length (take5 s) <= 5)
  deepCheck (\s -> all (`elem` ['a'..'e']) (take5 s))
  where deepCheck = quickCheckWith (stdArgs {maxSuccess = 10000})
