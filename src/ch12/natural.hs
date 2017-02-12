module Natural where

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

exampleNat = Succ (Succ (Succ Zero))

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n == 0    = Just Zero
  | n < 0     = Nothing
  | otherwise = Just (go n)
    where
      go 0 = Zero
      go n = Succ (go (n - 1))
