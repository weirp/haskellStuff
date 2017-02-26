module Case where

functionC x y = if (x > y) then x else y
functionC' x y =
  case x > y of
    True -> x
    False -> y

ifEvenAdd2 n = if even n then (n+2) else n
ifEvenAdd2' n =
  case even n of
    True -> (n+2)
    _ -> n

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0


returnLast :: a -> b -> c -> d -> d
returnLast _ _ _ d = d

returnLast' :: a -> (b -> (c -> (d -> d)))
returnLast' _ _ _ d = d

data Employee = Coder
  | Manager
  | Veep
  | CEO
  deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
  putStrLn $ show e ++ " is the boss of " ++ show e'

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _ = GT
codersRuleCEOsDrool _ Coder = LT
codersRuleCEOsDrool e e' = compare e e'



employeeRank :: (Employee -> Employee -> Ordering)
  -> Employee
  -> Employee
  -> IO ()
employeeRank f e e' =
  case f e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee is the boss"
    LT -> (flip reportBoss) e e'

dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2
