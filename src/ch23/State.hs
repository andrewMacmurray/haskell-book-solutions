{-# LANGUAGE InstanceSigs #-}

module Ch23.State where

newtype Moi s a = Moi
  { runMoi :: s -> (a, s)
  }


instance Functor (Moi s) where
  fmap :: (a -> b) -> (Moi s a) -> (Moi s b)
  fmap f (Moi g) = Moi (\s -> let (a, s1) = g s
                               in (f a, s1))


instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi (\s -> (a, s))
  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (<*>) (Moi sab) (Moi fa) =
    Moi (\s -> let (a, s1) = fa s
                   (fab, s2) = sab s1
                in (fab a, s2))


instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (>>=) (Moi f) sab =
    Moi (\s -> let (a, s1) = f s
                in runMoi (sab a) s1)
