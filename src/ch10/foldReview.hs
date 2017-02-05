module FoldReview where

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f (x:xs) = foldr (\b a -> f b || a) (f x) xs

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f = foldr ((||) . f) False

-- myAny' (== 3) [ 1, 2, 3, 4, 5 ]
-- (1 == 3 (2 == 3 || (3 == 3 || (4 == 3 || (False || 5 == 3)))))
-- False || False || True || False || False

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr ((||) . (== x)) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = myAny' (== x)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\b a -> if f b == True then b : a else a) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldr (\b a -> if f a b == GT then a else b) x xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldr (\b a -> if f a b == GT then b else a) x xs
