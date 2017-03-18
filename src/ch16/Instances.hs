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

fSpec :: IO ()
fSpec = do
  quickCheck (functorIdentity :: Identity String -> Bool)
  quickCheck (functorCompose' :: IdentityCompose)
