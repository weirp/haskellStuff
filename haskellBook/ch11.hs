{-# LANGUAGE FlexibleInstances #-}

module Ch11 where
import Data.Char

data PugType = PugData

data HuskyType a = HuskyData

data DogueDeBordeaux doge = DogueDeBordeaux doge



myPug = PugData :: PugType

myHusky :: HuskyType a
myHusky = HuskyData

myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData

myOtherOtherHusky :: HuskyType [[[[[[Int]]]]]]
myOtherOtherHusky = HuskyData


myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10

{-
badDoge :: DogueDeBordeaux String
badDoge = DogueDeBordeaux 10
-}

data Doggies a
  = Husky a
  | Mastiff a
    deriving (Eq, Show)

--Doggies

data Price =
  Price Integer
  deriving (Eq, Show)

data Size =
  Size Integer
  deriving (Eq, Show)

data Manufacturer
  = Mini
  | Mazda
  | Tata
    deriving (Eq, Show)

data Airline
  = PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
    deriving (Eq, Show)

data Vehicle
  = Car Manufacturer Price
  | Plane Airline Size
    deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 82)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars xs = map isCar xs


getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu
getManu _ = undefined


{- newtype -}


newtype Goats =
  Goats Int deriving (Eq, Show)
newtype Coms =
  Cows Int deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

{-typeclass-}
class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany Goats where
  tooMany (Goats n) = n > 43

{- Logic Goats -}
instance TooMany (Int, String) where
  tooMany (n, _) = n > 44

instance TooMany (Int, Int) where
  tooMany (n1, n2) = (n1 + n2) > 42

{-
instance TooMany (Num a, TooMany a) => (a, a) where
  tooMany (x, y) = (x + y) > 42 -}


{- 11.13-}

data GuessWhat =
  Chickenbutt deriving (Eq, Show)
data Id a =
  MkId a deriving (Eq, Show)
data Product a b =
  Product a b deriving (Eq, Show)
data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)
data RecordProduct a b =
  RecordProduct
  { pfirst :: a
  , psecond :: b
  }



newtype NumCow =
  NumCow Int
  deriving (Eq, Show)
newtype NumPig =
  NumPig Int
  deriving (Eq, Show)
data Farmhouse =
  Farmhouse NumCow NumPig
  deriving (Eq, Show)
type Farmhouse' = Product NumCow NumPig

newtype NumSheep =
  NumSheep Int
  deriving (Eq, Show)
data BigFarmhouse =
  BigFarmhouse NumCow NumPig NumSheep
  deriving (Eq, Show)
type BigFarmhouse' =
  Product NumCow (Product NumPig NumSheep)

type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int
data CowInfo =
  CowInfo Name Age
  deriving (Eq, Show)
data PigInfo =
  PigInfo Name Age LovesMud
  deriving (Eq, Show)
data SheepInfo =
  SheepInfo Name Age PoundsOfWool
  deriving (Eq, Show)


data Animal
  = Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo
  deriving (Eq, Show)
-- Alternately
type Animal'
  = Sum CowInfo (Sum PigInfo SheepInfo)

{-
data Automobile
  = Null
  | Car' { make :: String
        , model :: String
        , year :: Integer
        }
    deriving (Eq, Show)
-}

data Car' = Car' { make :: String
                 , model :: String
                 , year :: Integer
                 }
            deriving (Eq, Show)

data Automobile
  = Null
  | Automobile Car'
    deriving (Eq, Show)

data Quantum
  = Yes
  | No
  | Both
    deriving (Eq, Show)

quantSum1 :: Either Quantum Quantum
quantSum1 = Right Yes

quantSum2 :: Either Quantum Quantum
quantSum2 = Right No

quantSum3 :: Either Quantum Quantum
quantSum3 = Right Both

quantSum4 :: Either Quantum Quantum
quantSum4 = Left Yes

quantProd1 :: (Quantum, Quantum)
quantProd1 = (Yes, No)

quantProd2 :: (Quantum, Quantum)
quantProd2 = (Yes, No)

quantProd3 :: (Quantum, Quantum)
quantProd3 = (Yes, Both)

quantProd4 :: (Quantum, Quantum)
quantProd4 = (No, Yes)

quantProd5 :: (Quantum, Quantum)
quantProd5 = (No, No)

quantProd6 :: (Quantum, Quantum)
quantProd6 = (No, Both)

quantProd7 :: (Quantum, Quantum)
quantProd7 = (Both, Yes)

quantumFlip1 :: Quantum -> Quantum
quantumFlip1 Yes = Yes
quantumFlip1 No = Yes
quantumFlip1 Both = Yes

quantFlip2 :: Quantum -> Quantum
quantFlip2 Yes = Yes
quantFlip2 No = Yes
quantFlip2 Both = No

quantFlip3 :: Quantum -> Quantum
quantFlip3 Yes = Yes
quantFlip3 No = Yes
quantFlip3 Both = Both

quantFlip4 :: Quantum -> Quantum
quantFlip4 Yes = Yes
quantFlip4 No = No
quantFlip4 Both = Yes

quantFlip5 :: Quantum -> Quantum
quantFlip5 Yes = Yes
quantFlip5 No = Both
quantFlip5 Both = Yes

quantFlip6 :: Quantum -> Quantum
quantFlip6 Yes = No
quantFlip6 No = Yes
quantFlip6 Both = Yes

quantFlip7 :: Quantum -> Quantum
quantFlip7 Yes = Both
quantFlip7 No = Yes
quantFlip7 Both = Yes

quantFlip8 :: Quantum -> Quantum
quantFlip8 Yes = Both
quantFlip8 No = Yes
quantFlip8 Both = No

quantFlip9 :: Quantum -> Quantum
quantFlip9 Yes = Both
quantFlip9 No = No
quantFlip9 Both = No

quantFlip10 :: Quantum -> Quantum
quantFlip10 Yes = Both
quantFlip10 No = No
quantFlip10 Both = Both


convert1 :: Quantum -> Bool
convert1 Yes = False
convert1 No = False
convert1 Both = False

convert2 :: Quantum -> Bool
convert2 Yes = False
convert2 No = False
convert2 Both = True

convert3 :: Quantum -> Bool
convert3 Yes = False
convert3 No = True
convert3 Both = False

convert4 :: Quantum -> Bool
convert4 Yes = True
convert4 No = False
convert4 Both = False

convert5 :: Quantum -> Bool
convert5 Yes = False
convert5 No = True
convert5 Both = True

convert6 :: Quantum -> Bool
convert6 Yes = True
convert6 No = True
convert6 Both = False

convert7 :: Quantum -> Bool
convert7 Yes = True
convert7 No = False
convert7 Both = True

convert8 :: Quantum -> Bool
convert8 Yes = True
convert8 No = True
convert8 Both = True

{-
yes it is Quantum -> Bool
cardinality : 3 -> 2
= 2 ^ 3 = 8
-}

-- Higher-kinded datatypes
data Silly a b c d = MkSilly a b c d deriving Show

{-
data EsResultFound a
  = EsResultFound
    { _version :: DocVersion
    , _source :: a
    } deriving (Eq, Show)
-}

{- 11.17 Binary Tree-}

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right)
  = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay =
  if (mapTree (+1) testTree' == mapExpected)
  then print "yup okay!"
  else error "test failed!"

-- convert binary trees to lists
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "postorder failed check!"

main :: IO()
main = do
  testPreorder
  testInorder
  testPostorder


foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f b a = foldr f b (inorder a)

{- 11.18 chapter exercises -}

data Weekday
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday

{- Ciphers -}

f :: Show a => (a,b) -> IO(a,b)
f t@(a,_) = do
  print a
  return t

doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:_) = x: xs

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf s [] = False
isSubsequenceOf s@(x1:x1s) x@(x2:x2s)
  | x1 == x2 = isSubsequenceOf x1s x2s
  | otherwise = isSubsequenceOf s x2s

capitaliseWords :: String -> [(String, String)]
capitaliseWords w = map f (words w)
  where f wrd@(firstLetter:otherLetters) = (wrd, [toUpper firstLetter] ++ otherLetters)

capitaliseWord :: String -> String
capitaliseWord wrd@(lh:lr) = [toUpper lh] ++ lr

capitaliseParagraph :: String -> String
capitaliseParagraph = undefined

{- phone exercise -}

data KeyInfo = KeyInfo Int [Char]
data DaPhone = DaPhone [KeyInfo]


convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol lol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok, do u think i am pretty lol"
  , "Lol ya"
  , "Haha thanks just making sure rofl ur turn"
  ]

type Digit = Char
type Presses = Int
