module Ch10 where

stops = "pbtdkg"
vowels = "aeiou"

nouns = ["cheese", "cat", "bird", "penguin", "patio", "pangolin", "chair"]
verbs = ["eat", "run", "catch", "buy"]

makeStopVowelStop :: String -> String -> [String]
makeStopVowelStop s v = [[a] ++ [b] ++ [c] | a <- s, b <- v, c <- s, a == 'p']


makeNvn n v = [[a] ++ [b] ++ [c] | a <- n, b <- v, c <- n]

seekritFunc x =
  (fromIntegral (sum (map length (words x))))
  /  (fromIntegral (length (words x)))

  {- avg word length-}


myOr :: [Bool] -> Bool
myOr = foldr
  (\a b ->
     if a == True
     then True
     else b) False

myOr' :: [Bool] -> Bool
myOr' = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr
  (\a b ->
     if (f a)
     then True
     else b) False


myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f = foldr
  (\a b ->
     if (f a)
     then True
     else b) False

myElem1 :: Eq a => a -> [a] -> Bool
myElem1 x = foldr
  (\a b -> x == a || b) False

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 x = any (== x)


myReverse :: [a] -> [a]
myReverse = foldr
  (\a b -> b ++ [a]) []


myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr
  (\a b -> [f a] ++ b) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr
  (\a b -> if f a
           then [a] ++ b
           else b) []

squish :: [[a]] -> [a]
squish = foldr
  (\a b -> a ++ b) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr
  (\a b -> f a ++ b) []

squishAgain :: [[a]] -> [a]
squishAgain = undefined

{- ok, catamorphisms-}
