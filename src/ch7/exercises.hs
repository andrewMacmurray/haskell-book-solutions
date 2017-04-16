module Exercises where

tensDigit :: Integral a => a -> a
tensDigit x = d
  where
    (xLast, _) = x `divMod` 10
    d          = xLast `mod` 10

foldBool :: a -> a -> Bool -> a
foldBool x y z =
  case z of
    True  -> x
    False -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y z
  | z == True  = x
  | otherwise  = y

g :: (a -> b) -> (a, c) -> (b, c)
g aTob (a, c) = (aTob a, c)
