module Main where

import Data.Monoid
import Control.Applicative
import Data.List (elemIndex)


f x = lookup x [(3, "hello"), (4, "julie"), (5, "kbai")]
g y = lookup y [(7, "sup?"), (8, "chris"), (9, "aloha")]

h z = lookup z [(2, 3), (5, 6), (7, 8)]
m x = lookup x [(4, 10), (8, 13), (1, 9001)]


added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1,2,3][4,5,6])

y :: Maybe Integer
y = lookup 3 $ zip [1,2,3][4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3][4,5,6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3

x' :: Maybe Int
x' = elemIndex 3 [1, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x' <*> y'

-- 4
xs = [1, 2, 3]
ys = [4, 5, 6]

x'' :: Maybe Integer
x'' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum <$> (,) <$> x'' <*> y''


-- Identity exercise
newtype Identity' a = Identity' a
  deriving (Eq, Ord, Show)
instance Functor Identity' where
  fmap f (Identity' a) = Identity' (f a)
instance Applicative Identity' where
  pure a = Identity' a
  (<*>) (Identity' f) (Identity' x) = Identity' (f x)

{-
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
-}

-- Constant instance

newtype Constant a b =
  Constant {getConstant :: a}
  deriving (Eq, Ord, Show)
instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a
instance (Monoid a) => Applicative (Constant a) where
  pure x = Constant mempty
  (<*>) (Constant x) (Constant y) = Constant (x `mappend` y)

--  (<*>) (Constant a) (Constant b) = Constant (a b)


-- Maybe Applicative
validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if (length s) > maxLen
  then Nothing
  else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

data Person =
  Person Name Address
  deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a =
  case mkName n of
    Nothing -> Nothing
    Just n' ->
      case mkAddress a of
        Nothing -> Nothing
        Just a' -> Just $ Person n' a'

data Cow = Cow
  { name :: String
  , age  :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0 = Just n
  | otherwise = Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty ->
              Just (Cow nammy agey weighty)


cowFromString' :: String -> Int -> Int -> Maybe Cow
cowFromString' name' age' weight' =
  Cow
  <$> noEmpty name'
  <*> noNegative age'
  <*> noNegative weight'

cowFromString'' :: String -> Int -> Int -> Maybe Cow
cowFromString'' name' age' weight' =
  liftA3 Cow (noEmpty name') (noNegative age') (noNegative weight')



cow1 :: Maybe (Int -> Int -> Cow)
cow1 = fmap Cow (noEmpty "Bess")

cow2 :: Maybe (Int -> Cow)
cow2 = cow1 <*> noNegative 1

maybeApply :: Maybe (a -> b) -> Maybe a -> Maybe b
maybeFmap  ::       (a -> b) -> Maybe a -> Maybe b

maybeApply = undefined
maybeFmap = undefined

-- Fixer Upper
--const <$> Just "Hello" <*> Just "World"
-- (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

-- 17.6 Applicative laws
mPure :: a -> Maybe a
mPure = pure

embed :: Num a => Maybe ((a -> b) -> b)
embed = mPure ($ 2)

mApply
  :: Maybe ((a -> b) -> b)
  -> Maybe  (a -> b)
  -> Maybe              b
mApply = (<*>)

myResult = pure ($ 2) `mApply` Just (+2)

-- interchange
{-
  also see sectioning
  e.g. f = (`subtract` 3)
       g = (3 `subtract`)
  partial application of infix operator....
  there's a gap; either to the left or right ...

used in interchange law for applicatives

pure ($ 2)

-}


main :: IO ()
main = do
  putStrLn "hello world"
