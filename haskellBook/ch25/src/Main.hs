{-# LANGUAGE InstanceSigs #-}
module Main where

import Control.Monad (join)

newtype Identity a =
  Identity { runIdentity :: a }
  deriving (Eq, Show)

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) =
    Compose $ (fmap . fmap) f fga

newtype One f a =
  One (f a)
  deriving (Eq, Show)
instance Functor f => Functor (One f) where
  fmap f (One fa) = One $ fmap f fa

newtype Three f g h a =
  Three (f (g (h a)))
  deriving (Eq, Show)
instance (Functor f, Functor g, Functor h) =>
         Functor (Three f g h) where
  fmap f (Three fgha) =
    Three $ (fmap . fmap . fmap) f fgha

{- Applicatives -}

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure x = Compose $ (pure . pure) x

  (Compose f) <*> (Compose a) =
    Compose $ ((<*>) <$> f) <*> a

-- Compose Foldable
instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose fga) =
    (foldMap . foldMap) f fga

{- traverse :: Applicative f => (a -> f b) -> t a -> f (t b) -}
instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga




--- onto Monad Transformer (25.7)
newtype MaybeIO a = MaybeIO {runMaybeIO :: IO (Maybe a)}
newtype MaybeList a = MaybeList {runMaybeList :: [Maybe a]}

-- IdentityT

-- Identity was:
newtype Identity' a = Identity' { runIdentity' :: a} deriving (Eq, Show)

newtype IdentityT f a =
  IdentityT {runIdentityT :: f a}
  deriving (Eq, Show)
-- functor
instance Functor Identity' where
  fmap f (Identity' a) = Identity' (f a)
instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)
-- applicative
instance Applicative Identity' where
  pure = Identity'
  (Identity' f) <*> (Identity' a) = Identity' (f a)
instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)
  (IdentityT fab) <*> (IdentityT fa) =
    IdentityT (fab <*> fa)
-- monad
instance Monad Identity' where
  return = pure
  (Identity' a) >>= f = f a
{-
instance (Monad m) => Monad (IdentityT m) where
  return = pure
  (IdentityT ma) >>= f =
    IdentityT $ ma >>= runIdentityT . f
-}
{-# LANGUAGE InstanceSigs #-}

instance (Monad m) => Monad (IdentityT m) where
  return = pure

  (>>=) :: IdentityT m a
    -> (a -> IdentityT m b)
    -> IdentityT m b
  (IdentityT ma) >>= f =
    IdentityT $ ma >>= runIdentityT . f


main :: IO ()
main = do
  putStrLn "hello world"
