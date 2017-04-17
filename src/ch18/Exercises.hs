module Ch18.Exercises where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Ch17.List

data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) _ _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq



data PhhhbtEither a b =
    Left' a
  | Right' b
  deriving (Eq, Show)

instance Functor (PhhhbtEither a) where
  fmap f (Left' a) = Left' a
  fmap f (Right' b) = Right' (f b)

instance Applicative (PhhhbtEither a) where
  pure = Right'
  (<*>) (Left' a) _ = Left' a
  (<*>) _ (Left' a) = Left' a
  (<*>) (Right' f) (Right' x) = Right' (f x)

instance Monad (PhhhbtEither a) where
  return = pure
  (>>=) (Left' a) _  = Left' a
  (>>=) (Right' b) f = f b

genEither :: (Arbitrary a, Arbitrary b) => Gen (PhhhbtEither a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left' a, Right' b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbtEither a b) where
  arbitrary = genEither

instance (Eq a, Eq b) => EqProp (PhhhbtEither a b) where
  (=-=) = eq


newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq


-- building from List instances in previous chapter

instance Monad List where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) (Cons a Nil) f = f a
  (>>=) (Cons a rest) f = f a `append` (rest >>= f)


testWithTrigger trigger = do
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger


monadSpec :: IO ()
monadSpec = do
  testWithTrigger (undefined :: Nope (Int, Int, Int))
  testWithTrigger (undefined :: PhhhbtEither String (Int, Int, Int))
  testWithTrigger (undefined :: Identity (Int, Int, Int))
  testWithTrigger (undefined :: List (String, String, String))
