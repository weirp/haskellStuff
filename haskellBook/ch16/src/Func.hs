{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Func where
import Test.QuickCheck
import Test.QuickCheck.Function
import GHC.Arr

functorCompose' :: (Eq (f c), Functor f) =>
       f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)


newtype Identity a = Identity a
instance Functor (Identity) where
  fmap f (Identity x) = Identity (f x)


type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

type StrToStr = Fun String String
type StrFC = [String] -> StrToStr -> StrToStr -> Bool

-- 2
data Pair a = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

functorIdentity2 :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity2 f = fmap id f == f

functorCompose2 :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose2 x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt2 = Fun Int Int
type IntFC2 = [Int] -> IntToInt2 -> IntToInt2 -> Bool

functorIdentityProp :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentityProp x = x == fmap id x


functorAssocProp :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorAssocProp f g x = (fmap (g . f) x) == (fmap g . fmap f $ x)


instance (Arbitrary a) => Arbitrary (Pair a)   where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Pair x y)

-- a type for the test .....
-- the signature .... final result of Bool
-- e.g.     Pair Int Int -> Pair Int Int -> Bool
type IntPairFmap = Pair Int -> Pair Int -> Bool



-- 3
data Two a b =
  Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Two x y)


-- 4
data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three x y z)

--5
data Three' a b =
  Three' a b b
  deriving (Eq, Show)
instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f  y) (f z)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three' x y z)

-- 6
data Four a b c d =
  Four a b c d
  deriving (Eq, Show)
instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
  => Arbitrary (Four a b c d) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Four w x y z)

-- 7
data Four' a b =
  Four' a b b b
  deriving (Eq, Show)
instance Functor (Four' a) where
  fmap f (Four' w x y z) = Four' w (f x) (f y) (f z)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Four' w x y z)


-- Maybe

incIfJust :: Num a => Maybe a -> Maybe a
incIfJust (Just n) = Just $ n + 1
incIfJust Nothing = Nothing

showIfJust :: Show a => Maybe a -> Maybe String
showIfJust (Just s) = Just $ show s
showIfJust Nothing = Nothing

--someFunc Nothing = Nothing
--someFunc (Just x) = Just $ someOtherFunc x

incMaybe :: Num a => Maybe a -> Maybe a
incMaybe m = fmap (+1) m

showMaybe :: Show a => Maybe a -> Maybe String
showMaybe s = fmap show s

incMaybe' :: Num a => Maybe a -> Maybe a
incMaybe' = fmap (+1)

showMaybe' :: Show a => Maybe a -> Maybe String
showMaybe' = fmap show

liftedInc :: (Functor f, Num b) => f b -> f b
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show



-- Possibly
data Possibly a
  = LolNope
  | Yeppers a
  deriving (Eq, Show)
instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers (f a)
instance (Arbitrary a) => Arbitrary (Possibly a) where
  arbitrary = do
    x <- arbitrary
    frequency [ (1, return LolNope)
              , (4, return (Yeppers x)) ]

-- Either
incIfRight :: Num a => Either e a -> Either e a
incIfRight (Right n) = Right $ n + 1
incIfRight (Left e)  = Left e

showIfRight :: Show a => Either e a -> Either e String
showIfRight (Right s) = Right $ show s
showIfRight (Left e) = Left e

incEither :: Num a => Either e a -> Either e a
incEither = fmap (+1)

showEither :: Show a => Either e a -> Either e String
showEither = fmap show

-- functor for sum
data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)
instance Functor (Sum a) where
  fmap _ (First x) = First x
  fmap f (Second x) = Second (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [ (1, return (First x))
              , (1, return (Second y))
              ]

-- constant
newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Show)
instance Functor (Constant m) where
  fmap _ (Constant v) = Constant v

-- 16.13
data Wrap f a =
  Wrap (f a)
  deriving (Eq, Show)
instance (Functor f) => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)

-- 16.14 IO Functor

-- 16.15

type Nat f g = forall a . f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- 16.17 Chapter exercises
--1
data Bool'
  = False
  | True
  deriving (Eq,Show)
-- no, kind is *, need * -> *

--2
data BoolAndSomethingElse a
  = False' a
  | True' a
  deriving (Eq, Show)
instance Functor (BoolAndSomethingElse) where
  fmap f (False' x) = False' (f x)
  fmap f (True' x) = True' (f x)

--3
data BoolAndMaybeSomethingElse a
  = Falsish
  | Truish a
  deriving (Eq, Show)
instance Functor (BoolAndMaybeSomethingElse) where
  fmap _ Falsish = Falsish
  fmap f (Truish x) = Truish (f x)

-- 4
--newtype Mu f =
--  Inf { outF :: f (Mu f)}

-- nah
--instance Functor (Mu) where
--  fmap = undefined

--5
data D =
  D (Array Word Word) Int Int
  deriving (Eq, Show)

-- rearranging
-- 1
data Sum' b a
  = First' a
  | Second' b
instance Functor (Sum' a) where
  fmap f (First' a) = First' (f a)
  fmap _ (Second' b) = Second' b

-- 2
data Company a c b
  = DeepBlue a c
  | Something b
instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- 3
data More b a
  = L a b a
  | R b a b
  deriving (Eq, Show)
instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- functor instances
-- 1
data Quant a b
  = Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)
instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

-- 2
data K a b =
  K a
  deriving (Eq, Show)
instance Functor (K a) where
  fmap _ (K a) = K a

-- 3
{-# LANGUAGE FlexibleInstances #-}
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)
newtype K' a b =
  K' a
  deriving (Eq, Show)
instance Functor (Flip K' a) where
  fmap f (Flip (K' a)) = Flip (K' (f  a))

-- 4
data EvilGoateeConst a b =
  GoateeConst b
  deriving (Eq, Show)
instance Functor (EvilGoateeConst a) where
  fmap f (GoateeConst b) = GoateeConst (f b)

-- 5
data LiftItOut f a =
  LiftItOut (f a)
instance (Functor f) => Functor (LiftItOut f)  where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)


main :: IO ()
main = do
  quickCheck (functorCompose' :: IntFC)
  quickCheck (functorCompose' :: StrFC)

  quickCheck $ \x -> functorIdentity2 (x :: [Int])
  quickCheck (functorIdentity2 :: [Int] -> Bool )
  quickCheck (functorCompose2 :: IntFC)

  putStrLn "Checking Pair"
  quickCheck (functorIdentityProp :: Pair Int -> Bool)
  quickCheck (functorAssocProp (+1) (*3) :: Pair Int -> Bool)

  putStrLn "Checking Two"
  quickCheck (functorIdentityProp :: Two String Int -> Bool)
  quickCheck (functorAssocProp (+1) (*3) :: Two Int Int -> Bool)

  putStrLn "Checking Three"
  quickCheck (functorIdentityProp :: Three String Int Int -> Bool)
  quickCheck (functorAssocProp (+1) (*3) :: Three Int Int Int -> Bool)

  putStrLn "Checking Three'"
  quickCheck (functorIdentityProp :: Three' String Int -> Bool)
  quickCheck (functorAssocProp (+1) (*3) :: Three' Int Int -> Bool)

  putStrLn "Checking Four"
  quickCheck (functorIdentityProp :: Four String Int Int String-> Bool)
  quickCheck (functorAssocProp (+1) (*3) :: Four String String Int Int -> Bool)

  putStrLn "Checking Four'"
  quickCheck (functorIdentityProp :: Four' String Int-> Bool)
  quickCheck (functorAssocProp (+1) (*3) :: Four' String Int -> Bool)

  putStrLn "Checking Possibly"
  quickCheck (functorIdentityProp :: Possibly Int-> Bool)
  quickCheck (functorAssocProp (+1) (*3) :: Possibly Int -> Bool)

  putStrLn "Checking Sum"
  quickCheck (functorIdentityProp :: Sum Int Int-> Bool)
  quickCheck (functorAssocProp (+1) (*3) :: Sum Int Int -> Bool)
