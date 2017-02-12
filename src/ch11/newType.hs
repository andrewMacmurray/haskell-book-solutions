{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module NewType where

newtype Cows =
  Cows Int
  deriving (Eq, Show)

newtype Group = Group (Int, Int) deriving (Show)

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany (n, x) = n > 42

instance TooMany (Int, Int) where
  tooMany (x, y) = x + y > 42

instance TooMany Group where
  tooMany (Group (x, y)) = x + y > 42

newtype Goats =
  Goats Int
  deriving (Eq, Show, TooMany)
