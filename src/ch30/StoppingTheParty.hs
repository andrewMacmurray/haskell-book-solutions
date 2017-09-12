module Ch30.StoppingTheParty where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad (forever)
import System.Random (randomRIO)

randomException :: IO ()
randomException = do
  i <- randomRIO (1, 10 :: Int)
  if i `elem` [1..5]
    then throwIO DivideByZero
    else throwIO StackOverflow


main :: IO ()
main = forever $ do
  let tryS :: IO () -> IO (Either SomeException ())
      tryS = try
  x <- tryS randomException
  putStrLn "Live to loop another day"
  threadDelay 1000000
