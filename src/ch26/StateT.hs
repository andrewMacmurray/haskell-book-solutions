{-# LANGUAGE InstanceSigs #-}

module Ch26.StateT where

newtype StateT s m a = StateT {
  runStateT :: s -> m (a, s)
}


instance Functor m => Functor (StateT s m) where
  fmap f (StateT sma) = StateT $ \s ->
    fmap (\(a, s') -> (f a, s')) $ sma s

instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (<*>) (StateT smab) (StateT sma) = StateT $ \s -> do
    (ab, s') <- smab s
    fmap (\(a, s'') -> (ab a, s'')) $ sma s'

instance Monad m => Monad (StateT s m) where
  return = pure
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (>>=) (StateT sma) f = StateT $ \s -> do
    (a, s') <- sma s
    runStateT (f a) s'
