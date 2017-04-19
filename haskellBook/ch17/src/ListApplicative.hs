module ListApplicative where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


{-
==================================================================

Have a look at

https://www.reddit.com/r/HaskellBook/comments/4q6skj/ch_17_list_applicative_exercise/

==================================================================
-}


data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)


take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 0 _ = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

instance Monoid a => Monoid (List a) where
  mempty = Nil
  mappend Nil x = x
  mappend x Nil = x
  mappend (Cons x xs) ys = Cons x $ xs `mappend` ys


instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f y) x = (flatMap (\x -> Cons (f x) Nil) x) `append` (y <*> x)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = Cons <$> arbitrary <*> arbitrary

{-
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    frequency [ (1, return Nil)
              , (3, return (Cons x Nil))
              ]
-}

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

repeatList :: a -> (List a)
repeatList x = xs
  where xs = Cons x xs

zipListWith :: (a -> b -> c) -> (List a) -> (List b) -> (List c)
zipListWith _ Nil _ = Nil
zipListWith _ _ Nil = Nil
zipListWith f (Cons a as) (Cons b bs) = Cons (f a b) (zipListWith f as bs)

instance Applicative ZipList' where
  pure x = ZipList' (repeatList x)
  ZipList' fs <*> ZipList' xs = ZipList' (zipListWith id fs xs)





append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f xs = concat' $ fmap f xs



main :: IO ()
main = undefined
{-
main = do
  quickBatch $ monoid (List   :: List Int)
-}
