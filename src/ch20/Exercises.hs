module Ch20.Exercises where

import Data.Monoid
import Data.Foldable
import Utils ((|>))

data Constant a b =
  Constant a
  deriving (Eq, Show)

instance Foldable (Constant a) where
  foldMap _ _ = mempty


data Two a b =
  Two a b

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b


data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c


data Three' a b =
  Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' _ b c) = (f b) <> (f c)


data Four' a b =
  Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' _ b c d) = (f b) <> (f c) <> (f d)


filterF :: (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)
