module ReplaceExperiment where

import Test.QuickCheck

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted :: (Functor f2, Functor f1, Functor f) =>
                f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

a = fmap (+1) $ read "[1]" :: [Int]
b =  (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])
c = fmap (*2) (\x -> x - 2) 1
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3]) 0

--e :: IO Integer
--e = let ioi = readIO "1" :: IO Integer
--        changed = fmap read ("123" ++) (show ioi)
--    in fmap (*3) changed


-- 16.9 : QuickCheck
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
  (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x =
  (fmap g (fmap f x)) == (fmap (g . f) x)

main :: IO ()
main = do
    putStr "replaceWithP' lms: "
    print (replaceWithP' lms)
    putStr "liftedReplace lms: "
    print (liftedReplace lms)
    putStr "liftedReplace' lms:"
    print (liftedReplace' lms)
    putStr "twiceLifted lms:   "
    print (twiceLifted lms)
    putStr "twiceLifted' lms:  "
    print (twiceLifted' lms)
    putStr "thriceLifted lms:  "
    print (thriceLifted lms)
    putStr "thriceLifted' lms: "
    print (thriceLifted' lms)
