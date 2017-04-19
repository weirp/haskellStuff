{-# LANGUAGE InstanceSigs #-}
module WarmUp where

import Data.Char
import Control.Applicative (liftA2)
import Control.Monad

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers


cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = rev <$> cap

tupled :: [Char] -> ([Char], [Char])
tupled s = (s, fmapped s)

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
  x <- id
  y <- composed
  return (x, y)

--tupled'' :: [Char] -> ([Char], [Char])


-- 22.5 Ask
newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id


newtype HumanName =
  HumanName String
  deriving (Eq, Show)
newtype DogName =
  DogName String
  deriving (Eq, Show)
newtype Address =
  Address String
  deriving (Eq, Show)
data Person =
  Person
  { humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)
data Dog =
  Dog
  { dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pers :: Person
pers =
  Person
  (HumanName "Big Bird")
  (DogName "Barkley")
  (Address "Sesame Street")

chris :: Person
chris = Person
  (HumanName "Chris Allen")
  (DogName "Papu")
  (Address "Austin")

-- without Reader
getDog :: Person -> Dog
getDog p =
  Dog (dogName p) (address p)

-- with Reader
getDogR :: Person -> Dog
getDogR =
  Dog <$> dogName <*> address


-- with Reader , alternate
getDogR' :: Person -> Dog
getDogR' =
  liftA2 Dog dogName address

-- exercise : reading comprehension

myLiftA2 :: Applicative f =>
  (a -> b -> c)
  -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b

asks :: (r -> a) -> Reader r a
asks f = Reader f

--

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ (f . ra)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \r -> a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b

  (Reader rab) <*> (Reader ra) =
    Reader $ \r -> rab r (ra r)


-- 22.7 The mondad of functions
foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+1) r

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy

-- implement reader monad
-- 1
instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a
    -> (a -> Reader r b)
    -> Reader r b
  (Reader ra) >>= aRb =
    Reader $ \r -> aRb ra


main :: IO ()
main = do
  let trigger = undefined :: Reader Int (Int, Int, [Int])
  putStrLn "hello world"
