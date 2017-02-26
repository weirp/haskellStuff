module Lists where

eftBool :: Bool -> Bool -> [Bool]
eftBool from to
  | from > to = []
  | from == to = [from]
  | otherwise = [from] ++ eftBool (succ from) to


eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd from to
  | from > to = []
  | from == to = [from]
  | otherwise = [from] ++ eftOrd (succ from) to


eftInt :: Int -> Int -> [Int]
eftInt from to
  | from > to = []
  | from == to = [from]
  | otherwise = [from] ++ eftInt (succ from) to

eftChar :: Char -> Char -> [Char]
eftChar from to
  | from > to = []
  | from == to = [from]
  | otherwise = [from] ++ eftChar (succ from) to


myWords :: String -> [String]
myWords [] = []
myWords (x : xs)
    | x == ' ' = myWords (dropWhile (== ' ') xs)
    | otherwise = [[x] ++ takeWhile (/= ' ') xs] ++ myWords (dropWhile (/= ' ') xs)
