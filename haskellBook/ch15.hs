module Ch15 where

import Data.Monoid

class Monoid' m where
  mempty' :: m
  mappend' :: m -> m -> m
  mconcat' :: [m] -> m
  mconcat' = foldr mappend' mempty'


data Booly a
  = False'
  | True'
  deriving (Eq, Show)

-- conjunction
instance Monoid (Booly a) where
  mempty = False'
  mappend False' _ = False'
  mappend _ False' = False'
  mappend True' True' = True'


data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada x = x
  mappend x Nada = x
  mappend (Only x) (Only y) = Only (x `mappend` y)

-- Madness
type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj =
  e <> "! he said " <>
  adv <> " as he jumped into his car " <>
  noun <> " and drove off with his " <>
  adj <> " wife."

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj =
  foldr  (++) []
              [ e
              ,  "! he said "
              ,  adv
              ,  " as he jumped into his car "
              ,  noun
              ,  " and drove off with his "
              ,  adj
              ,  " wife."
              ]
