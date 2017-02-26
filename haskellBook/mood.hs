module Mood where

data Mood = Blah | Woot
  deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _    = Blah

x = (+)

f1 xs = w `x` 1
  where w = length xs
