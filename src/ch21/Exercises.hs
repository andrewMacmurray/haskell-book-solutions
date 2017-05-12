module Ch21.Exercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid
import Control.Applicative

newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance Eq a => EqProp (Identity a) where
  (=-=) = eq


newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse f (Constant a) = pure $ Constant a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
  arbitrary = do
    a <- arbitrary
    return $ Constant a

instance (Eq a, Eq b) => EqProp (Constant a b) where
  (=-=) = eq


data Optional a =
    Nada
  | Yep a
  deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    elements [Nada, Yep a]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq


data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = f x <> (foldMap f xs)

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

genList :: Arbitrary a => Gen (List a)
genList = do
  a <- arbitrary
  b <- genList
  frequency [ (3, return $ Cons a b)
            , (1, return Nil)
            ]

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = genList

instance Eq a => EqProp (List a) where
  (=-=) = eq


data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = (Three a b) <$> f c

instance (Arbitrary a
        , Arbitrary b
        , Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq


data Three' a b =
  Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Foldable (Three' a) where
  foldMap f (Three' a b c) = (f b) <> (f c)

instance Traversable (Three' a) where
  traverse f (Three' a b c) = (Three' a) <$> (f b) <*> (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three' a b c

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq



data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty    = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node xs x ys) = Node (fmap f xs) (f x) (fmap f ys)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node xs y ys) = (foldMap f xs) <> (f y) <> (foldMap f ys)

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node xs y ys) = liftA3 Node (traverse f xs) (f y) (traverse f ys)


treeGen :: Arbitrary a => Gen (Tree a)
treeGen = do
  xs <- treeGen
  ys <- treeGen
  a <- arbitrary
  elements [ Leaf a
           , Empty
           , Node xs a ys
           ]

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = treeGen

instance Eq a => EqProp (Tree a) where
  (=-=) = eq


type Trigger = (Int, Int, [Int])

main :: IO ()
main = do
  let trigger = undefined
  quickBatch $ traversable (trigger :: Identity Trigger)
  quickBatch $ traversable (trigger :: Constant Int Trigger)
  quickBatch $ traversable (trigger :: Optional Trigger)
  quickBatch $ traversable (trigger :: List Trigger)
  quickBatch $ traversable (trigger :: Three Trigger Trigger Trigger)
  quickBatch $ traversable (trigger :: Three' Trigger Trigger)
  quickBatch $ traversable (trigger :: Tree Trigger)
