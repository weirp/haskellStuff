module Db where

import Data.Time

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
           (fromGregorian 1911 5 1)
           (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
           (fromGregorian 1921 5 1)
           (secondsToDiffTime 34123))
  , DbNumber 7282
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate [] = []
filterDbDate ((DbDate x):xs) = [x] ++ filterDbDate xs
filterDbDate (_:xs) = filterDbDate xs

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber [] = []
filterDbNumber ((DbNumber x):xs) = [x] ++ filterDbNumber xs
filterDbNumber (_:xs) = filterDbNumber xs

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent xs = foldr
  max (UTCTime (fromGregorian 1900 1 1) (secondsToDiffTime 34123))
  (filterDbDate xs)

sumDb :: [DatabaseItem] -> Integer
sumDb xs = foldr (+) 0 (filterDbNumber xs)

avgDb :: [DatabaseItem] -> Double
{- use fromIntegral to get from Integer to Double -}
avgDb xs = (fromIntegral $ sumDb xs)
  / (fromIntegral $ length $ filterDbNumber xs)



fibs = take 20 $ 1 : scanl (+) 1 fibs
fibsN x = fibs !! x
