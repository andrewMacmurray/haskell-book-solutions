module Ch16.Sum where

import Test.QuickCheck
import Test.QuickCheck.Function
import Ch16.FunctorLaws

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

sumGen :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGen = do
  a <- arbitrary
  b <- arbitrary
  elements [ First a
           , Second b ]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = sumGen

type SumCompose =
  Fun Int Int ->
  Fun Int Int ->
  Sum String Int ->
  Bool

fSumSpec :: IO ()
fSumSpec = do
  quickCheck (functorIdentity :: Sum Int Int -> Bool)
  quickCheck (functorCompose' :: SumCompose)
