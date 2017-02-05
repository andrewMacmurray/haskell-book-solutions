module Recur where

fac :: Integral a => a -> a
fac 1 = 1
fac n = n * fac (n - 1)

fib :: Integral a => a -> a
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

data DividedResult =
      Result Integer
    | DividedByZero
  deriving (Show)

dividedBy :: Integer -> Integer -> DividedResult
dividedBy num denom = go num denom 0
  where
    go n d x
      | d == 0    = DividedByZero
      | n < d     = Result x
      | d > 0     = go (n - d) d (x + 1)
      | otherwise = go (n + d) d (x - 1)

sumRecur :: (Eq a, Num a) => a -> a
sumRecur 1 = 1
sumRecur n = n + sumRecur (n - 1)

multRecur :: Integral a => a -> a -> a
multRecur a 1 = a
multRecur a b = a + (multRecur a (b - 1))
