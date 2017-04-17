module Ch20.Library where

import Utils ((|>))
import Data.Foldable
import Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x xs =
  xs
    |> toList
    |> map ((==) x)
    |> foldMap Any
    |> getAny


newtype Min a = Min { getMin :: Maybe a } deriving (Eq, Ord, Show)

instance Ord a => Monoid (Min a) where
  mempty = Min Nothing
  mappend a (Min Nothing) = a
  mappend (Min Nothing) a = a
  mappend (Min (Just x)) (Min (Just y))
    | x <= y = Min $ Just x
    | otherwise = Min $ Just y

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' xs
  | null xs   = Nothing
  | otherwise = x
    where
      x = xs
        |> toList
        |> map Just
        |> foldMap Min
        |> getMin
