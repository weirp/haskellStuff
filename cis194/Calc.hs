{-# OPTIONS_GHC -Wall #-}
module Calc where

import ExprT
import Log


{-
Exercise 1
Write Version 1 of the calculator: an evaluator for ExprT, with the
signature
eval :: ExprT -> Integer
For example, eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20.
-}

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add left right) = (eval left) + (eval right)
eval (Mul left right) = (eval left) * (eval right)
