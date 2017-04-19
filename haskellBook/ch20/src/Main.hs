module Main where

import Data.Foldable
import Data.Monoid

{-
class Foldable (t :: + -> *) where
  fold :: Data.Monoid.Monoid m => t m -> m
  foldMap :: Data.Monoid.Monoid m => (a -> m) -> t a -> m
-}

data Identity a =
  Identity a
  deriving (Eq, Show)

instance Foldable Identity where
  -- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
  foldr f z (Identity x) = f x z
  -- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

data Optional a
  = Nada
  | Yep a
  deriving (Eq, Show)
instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep x) = f x z

  foldl _ z Nada = z
  foldl f z (Yep x) = f z x

  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

-- exercises: library functions
-- implement in terms of foldMap or foldr
sum' :: (Foldable t, Num a) => t a -> a
sum' xs = foldr (+) 0 xs

sum'' :: (Foldable t, Num a) => t a -> a
sum'' a = getSum $ foldMap Sum a

product' :: (Foldable t, Num a) => t a -> a
product' a  = getProduct $ foldMap Product a

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' e xs = getAny $ foldMap (\x -> Any $ x == e) xs


main :: IO ()
main = do
  putStrLn "hello world"
