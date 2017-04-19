module BadMonad where

import Data.Monoid
import Control.Applicative
import Control.Monad -- ((>=>))

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data CountMe a =
  CountMe Integer a
  deriving (Eq, Show)

instance Functor CountMe where
  fmap f (CountMe i a) = CountMe i (f a)

instance Applicative CountMe where
  pure = CountMe 0
  CountMe n f <*> CountMe n' a =
    CountMe (n + n') (f a)

instance Monad CountMe where
  return = pure

  CountMe n a >>= f =
    let CountMe n' b = f a
    in CountMe (n + n') b

instance Arbitrary a => Arbitrary (CountMe a) where
  arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where (=-=) = eq

mcomp :: Monad m
  => (b -> m c)
  -> (a -> m b)
  -> a -> m c
mcomp f g a = g a >>= f


sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Hello! How old are you? "


{-
(>=>) :: Monad m => (a -> m b) -> (b     -> m  c)  -> a  -> m  c
           String -> IO String -> String -> IO a  String    IO a

-}


-- 18.8 Monad exercises
-- 1 Nope Monad

data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ (NopeDotJpg) = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
  return = pure
  NopeDotJpg >>= _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where (=-=) = eq


-- 2
data PhhhbbtttEither b a
  = Left' a
  | Right' b
  deriving (Eq, Show)
instance Functor (PhhhbbtttEither b) where
  fmap _ (Right' b) = Right' b
  fmap f (Left' a) = Left' (f a)
instance Applicative (PhhhbbtttEither b) where
  pure x = Left' x
  (<*>) _ (Right' b) = Right' b
  (<*>) (Right' b) _ = Right' b
  (<*>) (Left' f) (Left' a) = Left' (f a)
instance Monad (PhhhbbtttEither b) where
  return = pure
  (Right' b) >>= _ = Right' b
  (Left' a) >>= f = f a
instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [ (1, return (Left' x))
              , (1, return (Right' y))
              ]
instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where (=-=) = eq

-- 3 identity

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
instance Applicative Identity where
  pure x = Identity x
  (<*>) (Identity f) (Identity a) = Identity (f a)
instance Monad Identity where
  return = pure
  (Identity x) >>= f = f x
instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return (Identity x)
instance (Eq a) => EqProp (Identity a) where (=-=) = eq

-- 4 List
data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Monoid (List a) where
  mempty = Nil
  mappend a Nil = a
  mappend Nil a = a
  mappend (Cons x xs) ys = Cons x $ xs `mappend` ys

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f b) xs = (fmap f xs) <> (b <*> xs)
instance Monad List where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) (Cons x xs) f = f x <> (xs >>= f)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [ (1, return Nil)
              , (3, return (Cons x y) )
              ]
take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

instance (Eq a) => EqProp (List a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = take' 3000 xs
          ys' = take' 3000 ys

j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 _ Nil = Nil
l1 f (Cons x xs) = Cons (f x) (l1 f xs)

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 _ Nil _ = Nil
l2 _ _ Nil = Nil
l2 (Cons x xs) (Cons y ys) = Cons (f a b) (l2 f xs ys)

a :: Monad m => m a -> m (a -> b) -> m b
a = flip ap

meh :: Monad m => [a] => (a -> m b) -> m [b]
meh [] _ = []
meh (x:xs) f = Cons (f x) <*> meh xs f

flipType :: (Monad m) => [m a] -> m [a]
flipType = undefined


-- ===============================================================
main = do
  let trigger = undefined :: CountMe (Int, String, Int)
      nope = NopeDotJpg :: Nope (String,String,String)
      phhhtt = undefined :: PhhhbbtttEither String (Int, String, Int)
      ident = undefined :: Identity (Int, String, Int)
      lst = undefined :: List (Int, String, Int)
  putStrLn "\nrunning tests for CountMe"
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

  putStrLn "\nrunning tests for Nope"
  quickBatch $ functor nope
  quickBatch $ applicative nope
  quickBatch $ monad nope

  putStrLn "\ntests for PhhhbbtttEither"
  quickBatch $ functor phhhtt
  quickBatch $ applicative phhhtt
  quickBatch $ monad phhhtt

  putStrLn "\ntests for Identity"
  quickBatch $ functor ident
  quickBatch $ applicative ident
  quickBatch $ monad ident

  putStrLn "\ntests for List"
  quickBatch $ functor lst
  quickBatch $ applicative lst
  quickBatch $ monad lst
