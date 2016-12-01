module Homework4 where

{-
Exercise 1: Wholemeal programming
Reimplement each of the following functions in a more idiomatic
Haskell style. Use wholemeal programming practices, breaking each
function into a pipeline of incremental transformations to an entire
data structure. Name your functions fun1’ and fun2’ respectively.
1. fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
| even x = (x - 2) * fun1 xs
| otherwise = fun1 xs
2. fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n ‘div‘ 2)
| otherwise = fun2 (3 * n + 1)
Hint: For this problem you may wish to use the functions iterate
and takeWhile. Look them up in the Prelude documentation to see
what they do.
-}
fun1' :: [Integer] -> Integer
fun1' = product . (map (\x -> x - 2)) . filter even


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

{-
  We are only accumulating even inputs.
  generate even n -> (n `div` 2)
           otherwise (3 * n + 1)
  terminate when n = 1

-}


fun2' :: Integer -> Integer
fun2' n = sum $ filter even $ (takeWhile (> 1)
                  (iterate fn2_helper n))


fn2_helper :: Integer -> Integer
fn2_helper n
  | even n = n `div` 2
  | otherwise = 3 * n + 1




{- folding with trees -}

{-
Recall the definition of a binary tree data structure. The height of http://en.wikipedia.org/wiki/
Binary_tree a binary tree is the length of a path from the root to the deepest
node. For example, the height of a tree with a single node is 0; the
height of a tree with three nodes, whose root has two children, is 1;
and so on. A binary tree is balanced if the height of its left and right
subtrees differ by no more than 1, and its left and right subtrees are
also balanced.
You should use the following data structure to represent binary
trees. Note that each node stores an extra Integer representing the
height at that node.
data Tree a = Leaf
| Node Integer (Tree a) a (Tree a)
deriving (Show, Eq)
For this exercise, write a function
cis 194: homework 4 2
foldTree :: [a] -> Tree a
foldTree = ...
which generates a balanced binary tree from a list of values using
foldr.
For example, one sample output might be the following, also visualized
at right:
D
E
A
G
H
B
F C
I
J
foldTree "ABCDEFGHIJ" ==
Node 3
(Node 2
(Node 0 Leaf ’F’ Leaf)
’I’
(Node 1 (Node 0 Leaf ’B’ Leaf) ’C’ Leaf))
’J’
(Node 2
(Node 1 (Node 0 Leaf ’A’ Leaf) ’G’ Leaf)
’H’
(Node 1 (Node 0 Leaf ’D’ Leaf) ’E’ Leaf))
Your solution might not place the nodes in the same exact order,
but it should result in balanced trees, with each subtree having a
correct computed height.
-}

{-
data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree (x:xs) =


insert :: a -> Tree a -> Tree a
insert a t = g where
  g (Leaf) = Node 0 Leaf a Leaf
  g (Node lvl left val right) = h where
    h
-}



{- =========================================== -}

{-
Exercise 3: More folds!
1. Implement a function
xor :: [Bool] -> Bool
which returns True if and only if there are an odd number of True
values contained in the input list. It does not matter how many
False values the input list contains. For example,
xor [False, True, False] == True
xor [False, True, False, False, True] == False
Your solution must be implemented using a fold.
2. Implement map as a fold. That is, complete the definition
map’ :: (a -> b) -> [a] -> [b]
map’ f = foldr ...
in such a way that map’ behaves identically to the standard map
function.
cis 194: homework 4 3
3. (Optional) Implement foldl using foldr. That is, complete
-}

xor :: [Bool] -> Bool
xor xs = odd $ length $ filter (== True) xs

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x -> (\y -> (f x) : y)) [] xs
