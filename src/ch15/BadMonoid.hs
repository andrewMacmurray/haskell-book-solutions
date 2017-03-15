module Ch15.BadMonoid where

import Data.Monoid
import Control.Monad
import Test.QuickCheck

import Ch15.MonoidLaws

data Bull =
    Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = elements [Fools, Twoo]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool


-- these tests will fail as Bull monoid instance is invalid 

main :: IO ()
main = do
  quickCheck (monoidAssoc :: BullMappend)
  quickCheck (monoidLeftIdentity :: Bull -> Bool)
  quickCheck (monoidRightIdentity :: Bull -> Bool)
