module Ch14.Arbitrary where

import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

data Trivial =
  Trivial
  deriving (Eq, Show)


trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
  arbitrary = trivialGen


data Identity a =
  Identity a
  deriving (Eq, Show)


identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)


instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

identityGenInt :: Gen (Identity Int)
identityGenInt = arbitrary



data Pair a b =
  Pair a b
  deriving (Eq, Show)


pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return (Pair a b)


instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = pairGen

pairGenIntString :: Gen (Pair Int String)
pairGenIntString = arbitrary


data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)


sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqual = do
  a <- arbitrary
  b <- arbitrary
  oneof [ return $ First a
        , return $ Second b
        ]

sumGenFirstPls :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenFirstPls = do
  a <- arbitrary
  b <- arbitrary
  frequency [ (10, return $ First a)
            , (1,  return $ Second b)
            ]

sumGenIntInt :: Gen (Sum Int Int)
sumGenIntInt = sumGenFirstPls


main :: IO ()
main = do
  sample trivialGen
  sample identityGenInt
  sample pairGenIntString
  sample sumGenIntInt
