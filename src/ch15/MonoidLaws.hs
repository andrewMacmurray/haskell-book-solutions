module Ch15.MonoidLaws where

import Data.Monoid
import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a


-- type S = String
--
-- main :: IO ()
-- main = do
--   quickCheck (monoidAssoc :: S -> S -> S -> Bool)
--   quickCheck (monoidLeftIdentity :: S -> Bool)
--   quickCheck (monoidRightIdentity :: S -> Bool)
