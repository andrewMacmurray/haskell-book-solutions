module Ch17.Lookups where

import Control.Applicative
import Data.List

xs = [1..3]
ys = [4..6]

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip xs ys)

lookupLists :: Integer -> Maybe Integer
lookupLists x = lookup x $ zip xs ys

y :: Maybe Integer
y = lookupLists 3

z :: Maybe Integer
z = lookupLists 2

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

x :: Maybe Int
x = elemIndex 3 [1..5]

a :: Maybe Int
a = elemIndex 4 [1..5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> a

summed :: Maybe Integer
summed = fmap sum $ (,) <$> y <*> z
