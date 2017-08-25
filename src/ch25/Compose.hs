{-# LANGUAGE InstanceSigs #-}

module Ch25.Compose where

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) =
    Compose $ (fmap . fmap) f fga


newtype One f a =
  One (f a)
  deriving (Eq, Show)

instance Functor f => Functor (One f) where
  fmap f (One fa) = One $ fmap f fa


newtype Three f g h a =
  Three (f (g (h a)))
  deriving (Eq, Show)


instance (Functor f, Functor g, Functor h) => Functor (Three f g h) where
  fmap f (Three fgha) =
    Three $ (fmap . fmap . fmap) f fgha


instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose (pure . pure $ a)
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (<*>) (Compose f) (Compose fga) =
    Compose $ ((<*>) <$> f <*> fga)


instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose fga) = (foldMap . foldMap) f fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga


class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id


data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)
  first f (Deux a b) = Deux (f a) b
  second f (Deux a b) = Deux a (f b)

data Const a b = Const a

instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a)
  first f (Const a) = Const (f a)
  second _ (Const a) = Const a


data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)
  first f (Drei a b c) = Drei a (f b) c
  second f (Drei a b c) = Drei a b (f c)


data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)
  first f = bimap f id
  second _ (SuperDrei a b) = SuperDrei a b


data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a
  first _ (SemiDrei a) = SemiDrei a
  second _ (SemiDrei a) = SemiDrei a


data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)
  first f (Quadzzz a b c d) = Quadzzz a b (f c) d
  second f (Quadzzz a b c d) = Quadzzz a b c (f d)


data Either' a b =
    Left' a
  | Right' b

instance Bifunctor Either' where
  bimap f _ (Left' a) = Left' (f a)
  bimap _ g (Right' b) = Right' (g b)
  first f (Left' a) = Left' (f a)
  first _ (Right' b) = Right' b
  second _ (Left' a) = Left' a
  second f (Right' b) = Right' (f b)
