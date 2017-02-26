module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen
  ++ thirdSen ++ fourthSen


myLines :: String -> [String]
myLines [] = []
myLines (x : xs)
  | x == '\n' = myLines (dropWhile (== '\n') xs)
  | otherwise = [[x] ++ takeWhile (/= '\n') xs] ++ myLines (dropWhile (/= '\n') xs)

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

myBreak :: Char -> String -> [String]
myBreak ch [] = []
myBreak ch (x : xs)
  | x == ch = myBreak ch (dropWhile (== ch) xs)
  | otherwise = [[x] ++ takeWhile (/= ch) xs] ++
                myBreak ch (dropWhile (/= ch) xs)

main :: IO()
main = do
  print $ "Are they equal? "
    ++ show (myLines sentences == shouldEqual)
  print $ "Are they equal? "
    ++ show (myBreak '\n' sentences == shouldEqual)
