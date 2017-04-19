module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 21.6
--stringToMorse :: String -> Maybe [Morse]

-- 21.7
data Query     = Query
data SomeObj   = SomeObj
data IoOnlyObj = IoOnlyObj
data Err       = Err

decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

fetchFn :: Query -> IO [String]
fetchFn = undefined

makeIoOnlyObj :: [SomeObj]
              -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  case sequence (map decodeFn a) of
    (Left err) -> return $ Left $ err
    (Right res) -> do
      a <- makeIoOnlyObj res
      return $ Right a

pipelineFn' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn' =
  (traverse makeIoOnlyObj . mapM decodeFn =<<) . fetchFn

pipelineFn'' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn'' = (traverse makeIoOnlyObj
               . traverse decodeFn =<<) . fetchFn



-- Either
data Either' a b
  = Left' a
  | Right' b
  deriving (Eq, Ord, Show)
instance Functor (Either' a) where
  fmap _ (Left' x) = Left' x
  fmap f (Right' y) = Right' (f y)
instance Applicative (Either' e) where
  pure = Right'
  Left' e <*> _ = Left' e
  Right' f <*> r = fmap f r
instance Foldable (Either' a) where
  foldMap _ (Left' _) = mempty
  foldMap f (Right' y) = f y
  foldr _ z (Left' _) = z
  foldr f z (Right' y) = f y z
instance Traversable (Either' a) where
  traverse _ (Left' x) = pure (Left' x)
  traverse f (Right' y) = Right' <$> f y

-- tuple
{-
instance Functor ((,) a) where
  fmap f (x,y) = (x, f y)
instance Monoid a => Applicative ((,) a) where
  pure x = (mempty, x)
  (u, f) <*> (v, x) = (u `mappend` v, f x)
instance Foldable ((,) a) where
  foldMap f (_,y) = f y
  foldMap f z (_, y) = f y z
instance Traversable ((,) a) where
  traverse f (x, y) = (,) x <$> f y
-}

-- 21.11 quality control
type TI = []


-- 21.12 Exercises
-- Identity
newtype Identity' a = Identity' a
  deriving (Eq, Ord, Show)
instance Functor Identity' where
  fmap f (Identity' a) = Identity' (f a)
instance Foldable Identity' where
  foldMap f (Identity' a) = f a
instance Traversable Identity' where
  traverse f (Identity' x) = Identity' <$> f x
instance Arbitrary a => Arbitrary (Identity' a) where
  arbitrary = do
    x <- arbitrary
    return (Identity' x)
instance (Eq a) => EqProp (Identity' a) where (=-=) = eq


-- Constant
newtype Constant a b =
  Constant {getConstant :: a}
  deriving (Eq, Show)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
  arbitrary = do
    x <- arbitrary
    return (Constant x)

instance Functor (Constant a) where
  fmap f (Constant b) = Constant b
instance Foldable (Constant a) where
  foldMap f (Constant a) = mempty
instance Traversable (Constant a) where
  traverse f (Constant b) = pure $ Constant b
instance (Eq a) => EqProp (Constant a b) where (=-=) = eq


-- Maybe
data Optional a
  = Nada
  | Yep a
  deriving (Eq, Show)
instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep (f a)

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    x <- arbitrary
    frequency [ (1, return Nada)
              , (2, return (Yep x))
              ]
instance (Eq a) => EqProp (Optional a) where (=-=) = eq
instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a
instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a




main :: IO ()
main = do
  let trigger = undefined :: TI (Int, Int, [Int])
      t2 = undefined :: Identity' (Int, Int, [Int])
      t3 = undefined :: Constant Int (Int, Int, [Int])
      t4 = undefined :: Optional (Int, Int, [Int])
  putStrLn "hello world"
  quickBatch (traversable trigger)
  quickBatch (traversable t2)

  quickBatch $ functor t2

  putStrLn "\nTests for Constant"
  quickBatch $ traversable t3

  putStrLn "\ntesting Optional"
  quickBatch $ functor t4
  quickBatch $ traversable t4
