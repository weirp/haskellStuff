module Ch7 where

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d = xLast `mod` 10

tensDigit' x = d
  where xLast = fst $ x `divMod` 10
        d = snd $ xLast `divMod` 10

hunsD x = d2
  where xLast = fst $ x `divMod` 100
        d2 = snd $ xLast `divMod` 10

foldBool :: a -> a -> Bool -> a
foldBool = error "Error: need to implement foldBool!"

{- case version-}
foldBool1 :: a -> a -> Bool -> a
foldBool1 x y b = case b of
  True -> x
  False -> y

{- guard version-}
foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b
  | b = x
  | not b = y

{- pattern matching version-}
foldBool3 :: a -> a -> Bool -> a
foldBool3 x y True = x
foldBool3 x y False = y



g :: (a -> b) -> (a, c) -> (b, c)
g ab (a, c) = (ab a, c)
