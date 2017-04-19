module Exercises where

import Control.Monad
import Control.Monad.Trans.State hiding (get, put, exec, eval, modify)

get :: State s s
get = state $ \s -> (s, s)

put :: s -> State s ()
put = \s -> state $ \x -> ((), s)

{-
exec :: State s a -> s -> s
exec (State sa) s = undefined

eval :: State s a -> s -> a
eval (State sa) s = undefined
-}
modify :: (s -> s) -> State s ()
modify f = state $ \s -> ((), f s)


main :: IO ()
main = do
  putStrLn "ss"
