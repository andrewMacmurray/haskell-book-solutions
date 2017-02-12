module Anamorphs where

-- anamporhisms : where structures are built up
-- catamorphisms : where structures are broken down

-- iterator = myIterate (+1) 0
-- take 10 iterator = [0,1,2,3,4,5,6,7,8,9]
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)


-- unfolder = myUnfoldr (\x -> Maybe (x, x + 1)) 0
-- take 10 unfolder = [0,1,2,3,4,5,6,7,8,9]
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x =
  case f x of
    Nothing     -> []
    Just (a, b) -> a : myUnfoldr f b

-- version of iterate using myUnfoldr
betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\b -> Just (b, f b)) x
