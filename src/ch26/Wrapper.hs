module Ch26.Wrapper where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader


embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1

unwrapMaybs :: ExceptT String (ReaderT () IO) (Maybe Int)
unwrapMaybs = runMaybeT embedded


unwrapExcept :: ReaderT () IO (Either String (Maybe Int))
unwrapExcept = runExceptT unwrapMaybs

unwrapReader :: () -> IO (Either String (Maybe Int))
unwrapReader = runReaderT unwrapExcept


reEmbed :: MaybeT (ExceptT String (ReaderT () IO)) Int
reEmbed = MaybeT . ExceptT . ReaderT $ const (return (Right (Just 1)))
