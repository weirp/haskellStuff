{-# LANGUAGE InstanceSigs #-}

module Main where

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) =
    MaybeT $ (fmap . fmap) f ma
instance (Applicative m) => Applicative (MaybeT m) where
  pure x = MaybeT (pure (pure x))
  (MaybeT fab) <*> (MaybeT mma) =
    MaybeT $ ((<*>) <$> fab <*> mma)


newtype Identity a =
  Identity { runIdentity :: a }
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)
instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

innerMost :: [Maybe (Identity (a -> b))]
  -> [Maybe (Identity a -> Identity b)]
innerMost = (fmap . fmap) (<*>)

second' :: [Maybe (Identity a -> Identity b)]
  -> [Maybe (Identity a) -> Maybe (Identity b)]
second' = fmap (<*>)

final' :: [Maybe (Identity a) -> Maybe (Identity b)]
  -> [Maybe (Identity a)] -> [Maybe (Identity b)]
final' = (<*>)

lmiApply :: [Maybe (Identity (a -> b))]
  -> [Maybe (Identity a)]
  -> [Maybe (Identity b)]
lmiApply f x =
  final' (second' (innerMost f)) x

{-
instance (Applicative m) => Applicative (MaybeT m) where
  pure x = MaybeT (pure (pure x))
  (MaybeT fab) <*> (MaybeT mma) =
    MaybeT $ (<*>) <$> fab <*> mma
-}

instance (Monad m) => Monad (MaybeT m) where
  return = pure
  (>>=) :: MaybeT m a
    -> (a -> MaybeT m b)
    -> MaybeT m b
  (MaybeT ma) >>= f =
   MaybeT $ do
    v <- ma
    case v of
      Nothing -> return Nothing
      Just y -> runMaybeT (f y)


{- EitherT -}

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema

instance Applicative m => Applicative (EitherT e m) where
  pure = undefined
  f <*> a = undefined
instance Monad m => Monad (EitherT e m) where
  return = pure
  v >>= f = undefined
swapEitherT :: (Functor m) => EitherT e m a
  -> EitherT a m e
swapEitherT = undefined


main :: IO ()
main = do
  putStrLn "hello world"
