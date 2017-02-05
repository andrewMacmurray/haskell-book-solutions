module Reimplement where

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x == True
  then True
  else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) = if f x == True
  then True
  else myAny f xs


myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem y (x:xs) = if y == x
  then True
  else myElem y xs

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 x xs = myAny (== x) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap id xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = go f xs x
  where
    go g (y:ys) z =
      case g y z of
        GT -> go g ys y
        LT -> go g ys z
        EQ -> go g ys y
    go g [] z = z

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = go f xs x
  where
    go g (y:ys) z =
      case g y z of
        GT -> go g ys z
        LT -> go g ys y
        EQ -> go g ys z
    go g [] z = z

myMaximum = myMaximumBy compare

myMinimum = myMinimumBy compare
