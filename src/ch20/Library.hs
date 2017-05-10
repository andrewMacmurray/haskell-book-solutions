module Ch20.Library where

import Utils ((|>))
import Data.Foldable
import Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = getAny . foldMap (\y -> Any (y == x))

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = compare'' min

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = compare'' max

compare'' :: (Foldable t, Ord a) => (a -> a -> a) -> t a -> Maybe a
compare'' compare' xs
  | null xs   = Nothing
  | otherwise = xs' |> foldr compare' (head xs') |> Just
  where
    xs' = toList xs

null' :: (Foldable t) => t a -> Bool
null' x = length x == 0

length' :: (Foldable t) => t a -> Int
length' = foldr (\_ a -> a + 1) 0

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\b a -> (f b) <> a) mempty
