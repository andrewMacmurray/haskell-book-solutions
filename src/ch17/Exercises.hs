module Ch17.Exercises where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Pair a =
  Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure x = Pair x x
  (<*>) (Pair f g) (Pair x y) = Pair (f x) (g y)

genPair :: Arbitrary a => Gen (Pair a)
genPair = do
  a <- arbitrary
  return $ Pair a a

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = genPair

instance Eq a => EqProp (Pair a) where
  (=-=) = eq



data Two a b =
  Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure b = Two mempty b
  (<*>) (Two a f) (Two a' x) = Two (a <> a') (f x)

genTwo :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
genTwo = do
  a <- arbitrary
  b <- arbitrary
  return $ Two a b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = genTwo

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq


data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure c = Three mempty mempty c
  (<*>) (Three a b f) (Three a' b' x) = Three (a <> a') (b <> b') (f x)

genThree :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
genThree = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return $ Three a b c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = genThree

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq



data Three' a b =
  Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Monoid a => Applicative (Three' a) where
  pure b = Three' mempty b b
  (<*>) (Three' a f f') (Three' a' x x') = Three' (a <> a') (f x) (f' x')

genThree' :: (Arbitrary a, Arbitrary b) => Gen (Three' a b)
genThree' = do
  a <- arbitrary
  b <- arbitrary
  return $ Three' a b b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = genThree'

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq



data Four a b c d =
  Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure d = Four mempty mempty mempty d
  (<*>) (Four a b c f) (Four a' b' c' x) = Four (a <> a') (b <> b') (c <> c') (f x)

genFour :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
genFour = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return $ Four a b c d

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = genFour

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq



data Four' a b =
  Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Monoid a) => Applicative (Four' a) where
  pure b = Four' mempty mempty mempty b
  (<*>) (Four' a b c f) (Four' a' b' c' x) = Four' (a <> a') (b <> b') (c <> c') (f x)

genFour' :: (Arbitrary a, Arbitrary b) => Gen (Four' a b)
genFour' = do
  a <- arbitrary
  b <- arbitrary
  return $ Four' a a a b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = genFour'

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

x = ("a", "a", "a")

exerciseSpec :: IO ()
exerciseSpec = do
  quickBatch $ applicative (Pair x x)
  quickBatch $ applicative (Two x x)
  quickBatch $ applicative (Three x x x)
  quickBatch $ applicative (Three' x x x)
  quickBatch $ applicative (Four x x x x)
  quickBatch $ applicative (Four' x x x x)


-- combinations

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combinations :: [(Char, Char, Char)]
combinations = liftA3 (,,) stops vowels stops
