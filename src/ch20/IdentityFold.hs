module Ch20.IdentityFold where

data Identity a =
  Identity a
  deriving (Eq, Show)

-- Identity foldable less about combining values into a summary value
-- more about consuming the value inside the Identity constructor
instance Foldable Identity where
  foldr f x (Identity a) = f a x
  foldl f x (Identity a) = f x a
  foldMap f (Identity a) = f a
