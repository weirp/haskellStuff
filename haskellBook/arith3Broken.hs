module Arith3Broken where

main :: IO ()
main = do
  print (1 + 1)
  putStrLn (show 10)
  print (negate (-1))
  print ((+) 0 blah)
    where blah = negate 1
