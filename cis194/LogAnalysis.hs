{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log


parseMessage :: String -> LogMessage
parseMessage x = case x of
  ('I' : s) -> do
    let s2 = head (reads s :: [(TimeStamp, String)])
    (LogMessage Info (fst s2) (dropWhile (==' ') (snd s2)))
  ('W' : s) -> do
    let s2 = (head (reads s :: [(TimeStamp, String)]))
    (LogMessage Warning (fst s2) (dropWhile (==' ') (snd s2)))
  ('E' : s) -> do
    let s1 = (head (reads s :: [(Int, String)]))
    let s2 = (head (reads (snd s1) :: [(TimeStamp, String)]))
    (LogMessage (Error (fst s1)) (fst s2) (dropWhile (==' ')(snd s2)))
  _       -> Unknown "This is not in the right format"


parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

getTimeStamp :: LogMessage -> TimeStamp
getTimeStamp (LogMessage _ ts _) = ts
getTimeStamp _ = 0

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert newMsg (Node left msg right)
  | (getTimeStamp newMsg) == (getTimeStamp msg) = Node left newMsg right
  | (getTimeStamp newMsg) < (getTimeStamp msg) = Node (insert newMsg left) msg right
  | otherwise = Node left msg (insert newMsg right)
insert newMsg (Leaf) = Node Leaf newMsg Leaf


build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:[]) = Node Leaf x Leaf
build (x: ys) = (insert x (build ys))


testBuild :: Int -> IO MessageTree
testBuild n = do
  l <- (testParse parse n "error.log")
  return (build l)
