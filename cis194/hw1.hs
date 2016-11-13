toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n < 1 = []
  | n < 10 = [n]
  | otherwise =   (n `rem` 10) : (toDigitsRev (n `div` 10))


toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)


doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev (x: (y: zs)) = x:(y* 2):(doubleEveryOtherRev zs)
doubleEveryOtherRev (x: zs) = x:(doubleEveryOtherRev zs)


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther xs = (reverse (doubleEveryOtherRev (reverse xs)))


sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits xs = foldl (+) 0 xs


validate :: Integer -> Bool
validate n = ((sumDigits (doubleEveryOther (toDigits n))) `rem` 10) == 0


{- Towers of Hanoi -}

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 from to using = []
hanoi n from to using =
  (hanoi (n - 1) from using to) ++ ((from, to) : hanoi (n - 1) using to from)
