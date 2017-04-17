module Ch18.Exercises2 where

import Control.Monad
import Data.Monoid

j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip ap

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = liftM2 (++)
                      ((:[]) <$> (f x))
                      (meh xs f)

flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id
