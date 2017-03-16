module Ch15.Monoid where

import Test.QuickCheck
import Ch15.Semigroup
import Ch15.MonoidLaws
import Data.Semigroup

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)


instance (Monoid a, Semigroup a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)


mSpec :: IO ()
mSpec = do
  quickCheck (monoidAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (monoidAssoc :: IdentityAssoc)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)
