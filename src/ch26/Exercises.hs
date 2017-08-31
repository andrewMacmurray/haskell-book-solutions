module Ch26.Exercises where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.IO.Class
import Data.Functor.Identity

rDec :: Num a => Reader a a
rDec = reader $ flip (-) 1


rShow :: Show a => ReaderT a Identity String
rShow = reader show


liftGreet :: (Show a, MonadIO m) => a -> m ()
liftGreet x = liftIO . putStrLn $ "hello " ++ show x


rPintAndInc :: (Num a, Show a) => ReaderT a IO a
rPintAndInc = ReaderT $ \r -> do
  liftGreet r
  return (r + 1)


sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
  liftGreet s
  return (show s, s + 1)


isValid :: String -> Bool
isValid v = '!' `elem` v


maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- liftIO getLine
  guard $ isValid v
  return v


doExcite :: IO ()
doExcite = do
  putStrLn "say something exciting!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e  -> putStrLn ("Good, was very excite: " ++ e)
