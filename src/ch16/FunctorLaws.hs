module Ch16.FunctorLaws where

import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x =
  (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Functor f, Eq (f c)) => Fun a b -> Fun b c -> f a -> Bool
functorCompose' (Fun _ f) (Fun _ g) x =
  (fmap g (fmap f x)) == (fmap (g . f) x)
