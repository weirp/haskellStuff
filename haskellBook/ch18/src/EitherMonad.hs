module EitherMonad where

import Control.Applicative
import Data.Monoid (Monoid, (<>))

import Test.QuickCheck (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Checkers (quickBatch, eq, (=-=), EqProp)
import Test.QuickCheck.Classes (applicative, monad)


-- Either
{-
(>>=) :: Monad m => m a -> (a ->        m b) ->        m b
(>>=) ::     Either e a -> (a -> Either e b) -> Either e b

return :: Monad m => a ->        m a
return ::            a -> Either e a
-}

type Founded = Int
type Coders = Int

data SoftwareShop =
  Shop
  { founded     :: Founded
  , programmers :: Coders
  } deriving (Eq, Show)

data FoundedError
  = NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
  | n < 0     = Left $ NegativeYears n
  | n > 500   = Left $ TooManyYears n
  | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
  | n < 0     = Left $ NegativeCoders n
  | n > 500   = Left $ TooManyCoders n
  | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded     <- validateFounded years
  programmers <- validateCoders coders
  if programmers > div founded 10
    then Left $ TooManyCodersForYears founded programmers
    else Right $ Shop founded programmers

{-
Note that Either always short-circuits on the first thing to have
failed. It must because in the Monad, later values can depend on previ-
ous ones:
-}


data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [First a, Second b]

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure x = Second x
  (<*>) _ (First a) = First a
  (<*>) (First a) _ = First a
  (<*>) (Second f) (Second b) = Second (f b)

instance Monad (Sum a) where
  return = pure
  (>>=) (First a) _ = First a
  (>>=) (Second b) f = f b

instance (Eq a, Eq b) => EqProp (Sum a b) where (=-=) = eq

--main :: IO ()
--main = do
--  quickBatch $ applicative (Second ("b", "w", "1"))
--  quickBatch $ monad (Second ("b", "w", 1))
--  putStrLn "done."
