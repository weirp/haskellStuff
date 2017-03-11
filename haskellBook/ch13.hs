module Ch13 where

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid
  = NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name
  -> Age
  -> Either PersonInvalid Person

mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
    "Name was: " ++ show name ++
    "Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStr "name:"
  name <- getLine
  putStr "age:"
  ageStr <- getLine
  age <- read ageStr::Integer

  p <-  mkPerson name age
  return (case p of
    (Right p) -> ["Yay! "] ++ [show p]
    (Left e) -> ["Nah "] ++ [show e])
