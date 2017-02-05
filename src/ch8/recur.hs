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

dividedBy :: Int -> Int -> DividedResult
dividedBy _ 0 = DividedByZero
dividedBy num denom = go (abs num) (abs denom) 0
  where
    go n d x
      | num < 0 && denom < 0 && n < d = Result x
      | num > 0 && denom > 0 && n < d = Result x
      | num < 0 && denom > 0 && n < d = Result (negate x)
      | num > 0 && denom < 0 && n < d = Result (negate x)
      | otherwise = go (n - d) d (x + 1)

sumRecur :: (Eq a, Num a) => a -> a
sumRecur 1 = 1
sumRecur n = n + sumRecur (n - 1)

multRecur :: Integral a => a -> a -> a
multRecur a 1 = a
multRecur a b = a + (multRecur a (b - 1))

mc91 :: (Num a, Ord a) => a -> a
mc91 x
  | x > 100   = x - 10
  | otherwise = mc91 $ mc91 $ x + 11
