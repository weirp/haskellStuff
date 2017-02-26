module TypeKwonDo where

data Woot

data Blah

f1 :: Woot -> Blah
f1 = undefined

g1 :: (Blah, Woot) -> (Blah, Blah)
g1 (b, w) = (b, f1 w)


f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h x = g $ f x

data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e a = w $ q a


data X
data Y
data Z

xz :: X -> Z;xz = undefined
yz :: Y -> Z;yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)


munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge xy ywx x = fst $ ywx $ xy x
