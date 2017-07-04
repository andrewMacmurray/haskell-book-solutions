module Ch22.Exercises where

import Control.Applicative
import Data.Maybe
import Utils ((|>))

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

xs :: Maybe Integer
xs = zip x y |> lookup 3

ys :: Maybe Integer
ys = zip y z |> lookup 6

zs :: Maybe Integer
zs = zip x y |> lookup 4

z' :: Integer -> Maybe Integer
z' n = zip x z |> lookup n

x1 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs ys

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (z' n, z' n)

summed :: Num c => (c, c) -> c
summed (a, b) = a + b

bolt :: Integer -> Bool
bolt = liftA2 (&&) (>3) (<8)

seqA :: Integral a => a -> [Bool]
seqA m = sequenceA [(>3), (<8), even] m

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ s'
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ seqA 7
  -- solutions
  print $ and . seqA $ 2
  print $ seqA $ fromMaybe 0 s'
  print $ bolt $ fromMaybe 0 ys
