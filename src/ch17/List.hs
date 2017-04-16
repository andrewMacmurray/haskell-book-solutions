module Ch17.List where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a rest) = Cons (f a) (fmap f rest)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) xs = append (fmap f xs) (fs <*> xs)

genList :: Arbitrary a => Gen (List a)
genList = do
  a <- arbitrary
  l <- genList
  frequency [ (1, return Nil)
            , (2, return (Cons a l)) ]

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = genList

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' (fmap f xs)

instance Applicative ZipList' where
  pure x = ZipList' (Cons x Nil)
  (<*>) (ZipList' fs) (ZipList' xs) = ZipList' $ (zipListWith fs xs)

zipListWith :: List (a -> b) -> List a -> List b
zipListWith _ Nil = Nil
zipListWith Nil _ = Nil
zipListWith (Cons f Nil) (Cons x xs) = Cons (f x) (pure f <*> xs)
zipListWith (Cons f fs) (Cons x Nil) = Cons (f x) (fs <*> pure x)
zipListWith (Cons f fs) (Cons x xs)  = Cons (f x) (zipListWith fs xs)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs'`eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 1000 l
          ys' = let (ZipList' l) = ys
                in take' 1000 l

instance Eq a => EqProp (List a) where
    xs =-= ys = xs' `eq` ys'
        where xs' = let l = xs
                    in take' 1000 l
              ys' = let l = ys
                    in take' 1000 l

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

listSpec :: IO ()
listSpec = do
  quickBatch $ applicative (Cons ("a", "b", "c") Nil)
  quickBatch $ applicative (ZipList' (Cons ("a", "b", "b") Nil))
