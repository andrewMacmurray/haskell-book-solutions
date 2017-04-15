module Ch17.List where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a rest) = Cons (f a) (fmap f rest)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) xs = append (fmap f xs) (fs <*> xs)

genList :: Arbitrary a => Gen (List a)
genList = do
  a <- arbitrary
  l <- genList
  frequency [ (1, return Nil)
            , (5, return (Cons a l)) ]

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = genList

instance Eq a => EqProp (List a) where
    xs =-= ys = xs' `eq` ys'
        where xs' = let l = xs
                    in take' 3000 l
              ys' = let l = ys
                    in take' 3000 l

take' :: Integer -> List a -> List a
take' n xs = go n xs Nil
  where
    go _ Nil acc         = acc
    go n' (Cons h t) acc =
      if n' == 0
      then acc
      else go (n' - 1) t (Cons h acc)

main :: IO ()
main = quickBatch $ applicative (Cons ("a", "b", "c") Nil)
