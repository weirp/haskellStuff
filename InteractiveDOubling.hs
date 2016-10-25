import Text.Read

interactiveDoubling = do
  putStrLn "Choose a number:"
  s <- getLine
  let mx = readMaybe s :: Maybe Double
  case mx of
    Just x -> putStrLn ("The double of your number is " ++ show (2*x))
    Nothing -> do
      putStrLn "This is not a valid number. Retrying ..."
      interactiveDoubling

interactiveSumming = do
  sa <- getLine
  sb <- getLine
  let ma = readMaybe sa :: Maybe Double
      mb = readMaybe sb :: Maybe Double
  case ma of
    Just a -> case mb of
      Just b -> putStrLn ("The sum is " ++ show (a + b))
      Nothing -> retry
    Nothing -> retry
  where retry = do
          putStrLn "Invalid number"
          interactiveSumming

{- at ghci need to import Control.Applicative
to get result from:
:t (<*>)
-}
