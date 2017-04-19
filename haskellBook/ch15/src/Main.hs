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

instance S.Semigroup (Or a b) where
  Snd x <> _     = Snd x
  _     <> Snd y = Snd y
  _     <> Fst y = Fst y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = oneof [fmap Fst arbitrary, fmap Snd arbitrary]


type OrAssocStrStr = Or String String -> Or String String -> Or String String -> Bool

qcSemiOrStrStr :: IO ()
qcSemiOrStrStr = quickCheck (semigroupAssoc :: OrAssocStrStr)


-- diverge : some quickcheck tutorials, generators etc
-- https://www.stackbuilders.com/news/a-quickcheck-tutorial-generators

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

prop_ReverseReverseId :: [Integer] -> Bool
prop_ReverseReverseId xs =
  reverse' (reverse' xs) == xs


dice :: Gen Int
dice = choose (1, 6)

arbitraryBool :: Gen Bool
arbitraryBool = choose (False, True)

-- sized :: (Int -> Gen a_ => Gen a)

arbitraryList :: Arbitrary a => Gen [a]
arbitraryList = sized $
  \n -> do
    k <- choose (0, n)
    sequence [arbitrary | _ <- [1..k]]

data Tree a
  = Tree a [Tree a]
  deriving (Show)

aTree :: Tree Int
aTree =
  Tree 5 [Tree 12 [Tree (-16) []], Tree 10 [], Tree 16 [Tree 12 []]]

nodes :: Tree a -> Int
nodes (Tree p []) = 1
nodes (Tree p c@ (x:xs)) = 1 + (cntChldn c)
  where cntChldn c@(x:xs) = cntChldn xs + nodes x
        cntChldn [] = 0

edges :: Tree a -> Int
edges (Tree t []) = 0
edges (Tree t c@(x:xs)) = length c + cntChldrn c
  where cntChldrn [] = 0
        cntChldrn c@(x:xs) = edges x + cntChldrn xs

prop_OneMoreNodeThanEdges :: Tree Int -> Bool
prop_OneMoreNodeThanEdges tree =
  nodes tree == edges tree + 1

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary =
    sized arbitrarySizedTree

arbitrarySizedTree :: Arbitrary a => Int -> Gen (Tree a)
arbitrarySizedTree m = do
    t <- arbitrary
    n <- choose (0, m `div` 2)
    ts <- vectorOf n (arbitrarySizedTree (m `div` 4))
    return (Tree t ts)

-- diverge finished: quickcheck tutorials, generators etc

-- 9
--See: https://kseo.github.io/posts/2016-12-14-how-quick-check-generate-random-functions.html

newtype Combine a b =
  Combine { unCombine :: (a -> b)}

instance (S.Semigroup b) => S.Semigroup (Combine a b) where
  Combine f1 <> Combine f2 = Combine (f1 S.<> f2)

-- quickcheck for this !!

-- 10




-- Monoids

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty M.<> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = a == (mempty M.<> a)

monoidAssoc :: (Eq m, M.Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a M.<> (b M.<> c)) == ((a M.<> b) M.<> c)

-- M1

instance Monoid Trivial where
  mempty = Trivial
  mappend = (S.<>)

qcMonoidTrivial = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)


-- M2
newtype Identity' a = Identity' a
  deriving (Eq,  Show)

-- provide Monoid defn
instance (M.Monoid a) => M.Monoid (Identity' a) where
  mempty = Identity' mempty
  mappend (Identity' x) (Identity' y) = Identity' (x M.<> y)

-- for property testing
instance (Arbitrary a) => Arbitrary (Identity' a) where
  arbitrary = fmap Identity' arbitrary

-- this is the test-case, right????
type IdentityStringAssoc' = Identity' String -> Identity' String -> Identity' String -> Bool

qcMonoidIdentityString :: IO ()
qcMonoidIdentityString = do
  quickCheck (monoidAssoc :: IdentityStringAssoc')
  quickCheck (monoidLeftIdentity :: Identity' String -> Bool)
  quickCheck (monoidRightIdentity :: Identity' String -> Bool)


-- M3
data Two' a b = Two' a b deriving (Eq, Show)
instance (M.Monoid a, M.Monoid b) => M.Monoid (Two' a b) where
  mempty = Two' mempty mempty
  mappend (Two' a b) (Two' a' b') = Two' (a M.<> a') (b M.<> b')
-- to finish


-- M4
newtype BoolConj' =
  BoolConj' Bool
  deriving (Eq, Show)

instance Monoid BoolConj' where
  mempty = BoolConj' True
  mappend (BoolConj' b) (BoolConj' b') = BoolConj' (b && b')

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
