module Fold where

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f z [] = z
myFoldr f z (x:xs) = f x (myFoldr f z xs)

myFoldl :: (a -> b -> b) -> b -> [a] -> b
myFoldl f z [] = z
myFoldl f z (x:xs) = myFoldl f (f x z) xs


-- foldl (*) 1 [1..3]
-- (((1 * 1) * 2) * 3)

-- foldr (*) 1 [1..3]
-- (1 * (2 * (1 * 3)))
