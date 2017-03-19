module Ch16.Instances where

import Test.QuickCheck
import Test.QuickCheck.Function
import Ch16.FunctorLaws

newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

type IdentityCompose =
  Fun String String ->
  Fun String String ->
  Identity String ->
  Bool


data Pair a =
  Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

pairGen :: Arbitrary a => Gen (Pair a)
pairGen = do
  a <- arbitrary
  return (Pair a a)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = pairGen

type PairCompose =
  Fun Bool String ->
  Fun String String ->
  Pair Bool ->
  Bool


data Two a b =
  Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

type TwoCompose =
  Fun String String ->
  Fun String String ->
  Two Int String ->
  Bool


data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (Three a b c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = threeGen

type ThreeCompose =
  Fun Int Int ->
  Fun Int Int ->
  Three String String Int ->
  Bool


data Three' a b =
  Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

threeGen' :: (Arbitrary a, Arbitrary b) => Gen (Three' a b)
threeGen' = do
  a <- arbitrary
  b <- arbitrary
  return (Three' a b b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = threeGen'

type ThreeCompose' =
  Fun Char Char ->
  Fun Char Bool ->
  Three' Char Char ->
  Bool


data Four a b c d =
  Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

fourGen :: ( Arbitrary a
           , Arbitrary b
           , Arbitrary c
           , Arbitrary d ) => Gen (Four a b c d)
fourGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return (Four a b c d)

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c
         , Arbitrary d ) => Arbitrary (Four a b c d) where
  arbitrary = fourGen

type FourCompose =
  Fun Int Int ->
  Fun Int Int ->
  Four Char String Bool Int ->
  Bool


data Four' a b =
  Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

fourGen' :: (Arbitrary a, Arbitrary b) => Gen (Four' a b)
fourGen' = do
  a <- arbitrary
  b <- arbitrary
  return (Four' a a a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = fourGen'

type FourCompose' =
  Fun Int Int ->
  Fun Int Int ->
  Four' Char Int ->
  Bool


-- data Trivial = Trivial
-- cannot implement Functor for Trivial
-- Trivial is kind * and Functor requires a type to be kind * -> *


fSpec :: IO ()
fSpec = do
  quickCheck (functorIdentity :: Identity String -> Bool)
  quickCheck (functorCompose' :: IdentityCompose)
  quickCheck (functorIdentity :: Pair Bool -> Bool)
  quickCheck (functorCompose' :: PairCompose)
  quickCheck (functorIdentity :: Two Int String -> Bool)
  quickCheck (functorCompose' :: TwoCompose)
  quickCheck (functorIdentity :: Three String String Int -> Bool)
  quickCheck (functorCompose' :: ThreeCompose)
  quickCheck (functorIdentity :: Three' Int Int -> Bool)
  quickCheck (functorCompose' :: ThreeCompose')
  quickCheck (functorIdentity :: Four Bool Int Char Bool -> Bool)
  quickCheck (functorCompose' :: FourCompose)
  quickCheck (functorIdentity :: Four' Char Int -> Bool)
  quickCheck (functorCompose' :: FourCompose')
