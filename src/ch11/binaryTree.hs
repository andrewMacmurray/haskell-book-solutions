module BinaryTree where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' x Leaf = Node Leaf x Leaf
insert' x (Node left y right)
  | x == y = Node left y right
  | x < y  = Node (insert' x left) y right
  | x > y  = Node left y (insert' x right)

mapTree :: Ord a => (a -> b) -> BinaryTree a -> BinaryTree b
mapTree f Leaf = Leaf
mapTree f (Node left x right) = Node (mapTree f left) (f x) (mapTree f right)

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

preOrder :: BinaryTree a -> [a]
preOrder Leaf = []
preOrder (Node left x right) = [x] ++ (preOrder left) ++ (preOrder right)

inOrder :: BinaryTree a -> [a]
inOrder Leaf = []
inOrder (Node left x right) = (inOrder left) ++ [x] ++ (inOrder right)

postOrder :: BinaryTree a -> [a]
postOrder Leaf = []
postOrder (Node left x right) = (postOrder left) ++ (postOrder right) ++ [x]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f x Leaf = x
foldTree f x (Node left y right) = f y (foldTree f (foldTree f x left) right)
