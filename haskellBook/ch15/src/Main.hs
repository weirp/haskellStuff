module Main where

import Test.QuickCheck
import qualified Data.Monoid as M
import qualified Data.Semigroup as S
import Test.QuickCheck hiding (Failure, Success)


data Trivial = Trivial deriving (Eq, Show)

instance S.Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, S.Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a S.<> (b S.<> c)) == ((a S.<> b) S.<> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2
newtype Identity a = Identity a
  deriving (Eq, Show)

-- provide Semigroup defn
instance (S.Semigroup a) => S.Semigroup (Identity a) where
  Identity x <> Identity y = Identity (x S.<> y)
-- for property testing
instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

-- this is the test-case, right????
type IdentityStringAssoc = Identity String -> Identity String -> Identity String -> Bool

qcSemiIdentityString :: IO ()
qcSemiIdentityString = quickCheck (semigroupAssoc :: IdentityStringAssoc)


-- 3
data Two a b = Two a b
  deriving (Eq, Show)

instance (S.Semigroup a, S.Semigroup b) => S.Semigroup (Two a b) where
  Two a b <> Two a' b' = Two (a S.<> a') (b S.<> b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b)   where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Two x y)

type TwoStringStringAssoc
  = Two String String
  -> Two String String
  -> Two String String
  -> Bool

qcSemiTwoStringString :: IO ()
qcSemiTwoStringString = quickCheck (semigroupAssoc :: TwoStringStringAssoc)

-- 4
data Three a b c = Three a b c deriving (Eq, Show)

instance (S.Semigroup a, S.Semigroup b, S.Semigroup c)
  => S.Semigroup (Three a b c) where
  Three a b c <> Three a' b' c' = Three (a S.<> a') (b S.<> b') (c S.<> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three x y z)

type ThreeStrStrStrAssoc
  = Three String String String
  -> Three String String String
  -> Three String String String
  -> Bool

qcSemiThreeStrStrStr :: IO ()
qcSemiThreeStrStrStr = quickCheck (semigroupAssoc :: ThreeStrStrStrAssoc)

-- 5
-- new data type
data Four a b c d = Four a b c d deriving (Eq, Show)
instance (S.Semigroup a, S.Semigroup b, S.Semigroup c, S.Semigroup d)
  => S.Semigroup (Four a b c d) where
  Four a b c d <> Four a' b' c' d'
    = Four (a S.<> a') (b S.<> b')
      (c S.<> c') (d S.<> d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
         => Arbitrary (Four a b c d) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Four w x y z)

-- type synonym
type FourStrStrStrStrAssoc
  = Four String String String String
  -> Four String String String String
  -> Four String String String String
  -> Bool

qcSemiFourStrStrStrStr :: IO ()
qcSemiFourStrStrStrStr = quickCheck (semigroupAssoc :: FourStrStrStrStrAssoc)

-- 6
-- create a new type from an existing one. BoolConj is the constructor
newtype BoolConj =
  BoolConj Bool deriving (Eq, Show)

instance S.Semigroup BoolConj where
  BoolConj b <> BoolConj b' = BoolConj (b && b')

instance Arbitrary BoolConj where
  arbitrary = fmap BoolConj (arbitrary :: Gen Bool)

type BoolConjAssoc
  = BoolConj -> BoolConj -> BoolConj -> Bool

qcBoolConj :: IO ()
qcBoolConj = quickCheck (semigroupAssoc :: BoolConjAssoc)

-- 7 BoolDisj
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)
instance S.Semigroup BoolDisj where
  BoolDisj b <> BoolDisj b' = BoolDisj (b || b')
instance Arbitrary BoolDisj where
  arbitrary = fmap BoolDisj (arbitrary :: Gen Bool)
type BoolDisjAssoc
  = BoolDisj -> BoolDisj -> BoolDisj -> Bool
qcBoolDisj :: IO ()
qcBoolDisj = quickCheck (semigroupAssoc :: BoolDisjAssoc)

-- 8 Or
data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

instance (S.Semigroup a, S.Semigroup b) => S.Semigroup (Or a b) where
  Snd b <> _     = Snd b
  _     <> Snd b = Snd b
  Fst a <> Fst b = Fst b
instance Arbitrary Or

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = oneof [fmap (First' . Only) arbitrary, return $ First' Nada]

instance Arbitrary Or where
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo) ]





main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
