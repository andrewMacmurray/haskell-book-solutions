module Ch16.Possibly where

import Test.QuickCheck
import Test.QuickCheck.Function
import Ch16.FunctorLaws

data Possibly a =
    LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope     = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

possiblyGen :: Arbitrary a => Gen (Possibly a)
possiblyGen = do
  a <- arbitrary
  elements [ LolNope
           , Yeppers a ]

instance Arbitrary a => Arbitrary (Possibly a) where
  arbitrary = possiblyGen

type PossiblyCompose =
  Fun Int Int ->
  Fun Int Int ->
  Possibly Int ->
  Bool

fPossiblySpec :: IO ()
fPossiblySpec = do
  quickCheck (functorIdentity :: Possibly Int -> Bool)
  quickCheck (functorCompose' :: PossiblyCompose)
