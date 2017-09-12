module Ch30.TryExcept where

import Control.Exception


onlyReportError :: Show e => IO (Either e a) -> IO ()
onlyReportError action = do
  res <- action
  case res of
    Left e  -> print e
    Right _ -> return ()

willFail :: Integer -> IO ()
willFail denom =
  onlyReportError $ willIFail denom


willIFail :: Integer -> IO (Either ArithException ())
willIFail denom =
  try $ print $ div 5 denom
