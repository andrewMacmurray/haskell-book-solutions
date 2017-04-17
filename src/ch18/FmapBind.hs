module Ch18.FmapBind where

import Control.Monad

bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f
