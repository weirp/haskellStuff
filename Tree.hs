data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)


treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f = g where
  g (Leaf x) = Leaf (f x)
  g (Branch left right) = Branch (g left) (g right)

treeFold :: (b -> b -> b) -> (a -> b) -> Tree a -> b
treeFold fbranch fleaf = g where
  g (Leaf x) = fleaf x
  g (Branch left right) = fbranch (g left) (g right)

tree1 :: Tree Integer
tree1 =
    Branch
       (Branch
           (Branch
               (Leaf 1)
               (Branch (Leaf 2) (Leaf 3)))
           (Branch
               (Leaf 4)
               (Branch (Leaf 5) (Leaf 6))))
       (Branch
           (Branch (Leaf 7) (Leaf 8))
           (Leaf 9))
