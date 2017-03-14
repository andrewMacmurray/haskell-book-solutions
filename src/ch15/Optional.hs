module Ch15.Optional where

import Data.Monoid

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)


instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada Nada = Nada
  mappend (Only a) Nada = Only a
  mappend Nada (Only a) = Only a
  mappend (Only a) (Only b) = Only (a <> b)
