module Ch15.Semigroup where

import Data.Semigroup
import Test.QuickCheck


data Trivial =
  Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc =
  Trivial ->
  Trivial ->
  Trivial ->
  Bool



newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

type IdentityAssoc =
  Identity String ->
  Identity String ->
  Identity String ->
  Bool



data Two a b =
  Two a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

type TwoAssoc =
  Two [Sum Int] String ->
  Two [Sum Int] String ->
  Two [Sum Int] String ->
  Bool



data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance
  ( Semigroup a
  , Semigroup b
  , Semigroup c ) => Semigroup (Three a b c) where
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (Three a b c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = threeGen

type ThreeAssoc =
  Three String String String ->
  Three String String String ->
  Three String String String ->
  Bool



data Four a b c d =
  Four a b c d
  deriving (Eq, Show)

instance
  ( Semigroup a
  , Semigroup b
  , Semigroup c
  , Semigroup d ) => Semigroup (Four a b c d) where
  (Four a b c d) <> (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
fourGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return (Four a b c d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = fourGen

type FourAssoc =
  Four String String String (Product Rational) ->
  Four String String String (Product Rational) ->
  Four String String String (Product Rational) ->
  Bool



newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  (BoolConj True) <> (BoolConj _)    = BoolConj False
  (BoolConj False) <> (BoolConj _)   = BoolConj False


boolConjGen :: Gen BoolConj
boolConjGen =
  elements [ BoolConj False, BoolConj True ]

instance Arbitrary BoolConj where
  arbitrary = boolConjGen

type BoolConjAssoc =
  BoolConj ->
  BoolConj ->
  BoolConj ->
  Bool




newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  (BoolDisj False) <> (BoolDisj _)     = BoolDisj True
  (BoolDisj True) <> (BoolDisj _)      = BoolDisj True

boolDisjGen :: Gen BoolDisj
boolDisjGen =
  elements [ BoolDisj False, BoolDisj True ]

instance Arbitrary BoolDisj where
  arbitrary = boolDisjGen

type BoolDisjAssoc =
  BoolDisj ->
  BoolDisj ->
  BoolDisj ->
  Bool


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a c b = (a <> (b <> c)) == ((a <> b) <> c)

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
