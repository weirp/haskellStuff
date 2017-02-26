module Ch9 where

import Data.Char

cap1 :: String -> String
cap1 [] = []
cap1 (x:xs) = [toUpper x] ++ xs


cap2 :: String -> String
cap2 [] = []
cap2 (x:xs) = [toUpper x] ++ cap2 xs


cap3 :: String -> Char
cap3 [] = ' '
cap3 (x:xs) = toUpper x
