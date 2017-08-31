{-# LANGUAGE OverloadedStrings #-}

module Ch26.Scotty where

import Web.Scotty
import Control.Monad.IO.Class
import Data.Monoid (mconcat)


main :: IO ()
main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    liftIO $ putStrLn "hello"
    html $
      mconcat
        [ "<h1>Scotty, "
        , beam
        , " me up!</h1>"
        ]
