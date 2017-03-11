module Main where

import qualified Data.Monoid as M
import qualified Data.Semigroup as S
import Test.QuickCheck hiding (Failure, Success)


data Bull
  = Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo) ]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

-- QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc x y z = x M.<> (y M.<> z) == (x M.<> y) M.<> z

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity x = (mempty M.<> x) == x

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity x = (x M.<> mempty) == x

type BullMappend = Bull -> Bull -> Bull -> Bool

qcMonoidAllBull :: IO ()
qcMonoidAllBull = do
  quickCheck (monoidAssoc :: BullMappend)
  quickCheck (monoidLeftIdentity :: Bull -> Bool)
  quickCheck (monoidRightIdentity :: Bull -> Bool)



-- Optional -- from ch15
data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada x = x
  mappend x Nada = x
  mappend (Only x) (Only y) = Only (x `mappend` y)

-- maybe another monoid

newtype First' a =
  First' { getFirst' :: Optional a}
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' Nada) (First' Nada) = First' Nada
  mappend (First' Nada) foy@(First' (Only _)) = foy
  mappend fox@(First' (Only _)) (First' Nada) = fox
  mappend (First' (Only x)) (First' (Only _)) = First' (Only x)

firstMappend :: First' a
             -> First' a
             -> First' a
firstMappend = mappend

type FirstMappend
     =  First' String
     -> First' String
     -> First' String
     -> Bool

type FstId =
  First' String -> Bool

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = oneof [fmap (First' . Only) arbitrary, return $ First' Nada]

qcFm :: IO ()
qcFm = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)


-- NonEmpty
data NonEmpty' a = a :| [a]
  deriving (Eq, Ord, Show)

main :: IO ()
main = do
  quickCheck (monoidAssoc :: BullMappend)
  quickCheck (monoidLeftIdentity :: Bull -> Bool)
  quickCheck (monoidRightIdentity :: Bull -> Bool)
