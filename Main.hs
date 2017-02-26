module Main where

import Data.Char (toUpper)
import Control.Monad
import Control.Applicative
import System.Random

main = putStrLn "Write your string: " >> fmap shout getLine >>= putStrLn

shout = map toUpper

rollDiceIO :: IO (Int, Int)
rollDiceIO = liftA2 (,) (randomRIO (1,6)) (randomRIO (1,6))

clumsyRollDice :: (Int, Int)
clumsyRollDice = (n, m)
  where
    (n, g) = randomR (1,6) (mkStdGen 0)
    (m, _) = randomR (1,6) g
