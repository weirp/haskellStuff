module Ch12 where

import Data.List

ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n = if even n then Just (n+ 2) else Nothing

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

mkPerson :: Name -> Age -> Maybe Person
mkPerson name age
  | name /= "" && age >= 0 = Just $ Person name age
  | otherwise = Nothing

data PersonInvalid = NameEmpty
  | AgeTooLow
   deriving (Eq, Show)

toString :: PersonInvalid ->String
toString NameEmpty =  "NameEmpty"
toString AgeTooLow = "AgeTooLow"

{-
instance Show PersonInvalid where
  show = toString
-}

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x = Just x

replaceThe' :: [String] -> String
replaceThe' [] = ""
replaceThe' ("the":xs) = "aaa " ++ replaceThe' xs
replaceThe' (x:xs) = (case (notThe x) of
                       Nothing -> "a "
                       Just x -> x ++ " ") ++ replaceThe' xs

replaceThe :: String -> String
replaceThe x = replaceThe' (words x)

isVowel :: Char -> Bool
isVowel x = (x == 'a' || x == 'e' || x == 'i' || x == 'o' || x == 'u')

countTheBeforeVowel' :: [String] -> Integer
countTheBeforeVowel' [] = 0
countTheBeforeVowel' ("the":(x:_):ys) = (if (isVowel x)
                                         then 1
                                         else 0) + countTheBeforeVowel' ys
countTheBeforeVowel' (x:xs) = countTheBeforeVowel' xs

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel x = countTheBeforeVowel' (words x)
countVowels :: String -> Integer
countVowels s = toInteger $ length . filter id $ map isVowel s

countNonVowels :: String -> Integer
countNonVowels s = toInteger (length s) - countVowels s

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord s
  | countVowels s > countNonVowels s = Nothing
  | otherwise = Just $ Word' s


data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x


integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n < 0 = Nothing
  | n == 0 = Just Zero
  | otherwise = Just (Succ res)
    where res = case (integerToNat (n-1)) of
            Nothing -> error "wat!"
            Just x -> x


{- small lib for maybe -}

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee dflt f Nothing = dflt
mayybee dflt f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe x (Just y) = y

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]


catMaybes :: [Maybe a] -> [a]
catMaybes = foldr
  (\a b -> case a of
    Just x -> [x] ++ b
    Nothing -> b) []

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = if (any isNothing xs) then Nothing else Just $ catMaybes xs

{- Either -}

lefts' :: [Either a b] -> [a]
lefts' = foldr
  (\a b -> case a of
      Left x -> [x] ++ b
      _ -> b) []

rights' :: [Either a b] -> [b]
rights' = foldr
  (\a b -> case a of
      Right x -> [x] ++ b
      _ -> b) []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs,rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just $ f b
eitherMaybe' _ _ = Nothing

{- catamorphism for either values -}
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fac _ (Left x) = fac x
either' _ fbc (Right x) = fbc x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' fbc x = either' (const Nothing) (Just . fbc) x

{- unfolds -}
mehSum :: Num a => [a] -> a
mehSum xs = go 0 xs
  where go :: Num a => a -> [a] -> a
        go n [] = n
        go n (x:xs) = (go (n+x) xs)

niceSum :: Num a => [a] -> a
niceSum = foldl' (+) 0

mehProduct :: Num a => [a] -> a
mehProduct xs = go 1 xs
  where go :: Num a => a -> [a] -> a
        go n [] = n
        go n (x:xs) = (go (n*x) xs)

niceProduct :: Num a => [a] -> a
niceProduct = foldl' (*) 1

mehConcat :: [[a]] -> [a]
mehConcat xs = go [] xs
  where go :: [a] -> [[a]] -> [a]
        go xs' [] = xs'
        go xs' (x:xs) = (go (xs' ++ x) xs)

niceConcat :: [[a]] -> [a]
niceConcat = foldr (++) []

myIterate :: (a -> a) -> a -> [a]
myIterate f n = [n] ++ myIterate f (f n)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = go f (f b) []
  where go:: (b -> Maybe (a, b)) -> Maybe (a, b) -> [a] -> [a]
        go f (Just (a, b)) a' = [a] ++ go f (f b) ([a])
        go _ Nothing _ = undefined

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\b -> Just (b, f b)) x


data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)


THIS IS UNFINISHED!!!!

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x = go f (f x) Leaf
  where go :: (a -> Maybe (a, b, a)) -> a -> Maybe (a, b, a) -> BinaryTree b -> BinaryTree b
        go f x res t =
{-
unfold f a = go f (f a) Leaf
  where go :: (a -> Maybe (a, b, a))
           -> Maybe (a, b, a)
           -> BinaryTree b
           -> BinaryTree b

        go f (Just a1, b, a2) t = undefined
        go _ Nothing _ = undefined
-}
