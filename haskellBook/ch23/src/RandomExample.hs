module RandomExample where

import System.Random

-- 6-sided die
data Die
  = DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDieThreeTimes :: Int -> (Die, Die, Die)
rollDieThreeTimes n = do
  let s = mkStdGen n
      (d1, s1) = randomR (1, 6) s :: (Int, StdGen)
      (d2, s2) = randomR (1, 6) s1 :: (Int, StdGen)
      (d3, _) = randomR (1, 6) s2 :: (Int, StdGen)
  (intToDie d1, intToDie d1, intToDie d3)
