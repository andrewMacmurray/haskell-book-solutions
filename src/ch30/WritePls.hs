module Ch30.WritePls where

import Control.Exception
import Data.Typeable


handler :: SomeException -> IO ()
handler (SomeException e) = do
  putStrLn ("Running main caused an error! it was: " ++ show e)
  writeFile "bbb" "hi"


main = do
  writeFile "zzz" "hi" `catch` handler
