module BinaryTree where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a =
  case f a of
    Nothing        -> Leaf
    Just (x, y, z) -> Node (unfold f x) y (unfold f z)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold treeBuilder 0
  where
    treeBuilder a
      | a < n     = Just (a + 1, a, a + 1)
      | otherwise = Nothing
