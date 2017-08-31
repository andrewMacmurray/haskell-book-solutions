module Ch26.EitherT where

newtype EitherT e m a = EitherT {
  runEitherT :: m (Either e a)
}


instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema


instance Applicative m => Applicative (EitherT e m) where
  pure a = EitherT $ (pure . pure) a
  (<*>) (EitherT f) (EitherT ema) =
    EitherT $ (<*>) <$> f <*> ema


instance Monad m => Monad (EitherT e m) where
  return = pure
  (>>=) (EitherT ema) f =
    EitherT $ ema >>= either (return . Left) (runEitherT . f)


swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) =
  EitherT $ swapEither <$> ema

swapEither :: Either e a -> Either a e
swapEither ea =
  case ea of
    Left e  -> Right e
    Right a -> Left a


eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT amc bmc (EitherT amb) =
  amb >>= either amc bmc
