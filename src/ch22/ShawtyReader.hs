{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BC
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.URI (URI, parseURI)
import qualified System.Random as SR
import Web.Scotty.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans

type ActionR = ActionT TL.Text (ReaderT R.Connection IO)
type ScottyR = ScottyT TL.Text (ReaderT R.Connection IO)

alphaNum :: String
alphaNum = ['A'..'Z'] ++ ['0'..'9']

randomElement :: String -> IO Char
randomElement xs = do
  let maxIndex :: Int
      maxIndex = length xs - 1
  randomDigit <- SR.randomRIO (0, maxIndex) :: IO Int
  return (xs !! randomDigit)

shortyGen :: IO String
shortyGen =
  replicateM 7 (randomElement alphaNum)


saveURI :: BC.ByteString
        -> BC.ByteString
        -> ActionR (Either R.Reply R.Status)
saveURI shortURI uri = runDb $ R.set shortURI uri


getURI  :: BC.ByteString
        -> ActionR (Either R.Reply (Maybe BC.ByteString))
getURI shortURI = runDb $ R.get shortURI


runDb :: R.Redis (Either R.Reply b) -> ActionR (Either R.Reply b)
runDb dbAction = do
  conn <- lift ask
  liftIO $ R.runRedis conn dbAction


linkShorty :: String -> String
linkShorty shorty =
  concat [ "<a href=\""
         , shorty
         , "\">Copy and paste your short URL</a>"
         ]

shortyCreated :: Show a => a -> String -> TL.Text
shortyCreated resp shawty =
  TL.concat [ TL.pack (show resp)
            , " shorty is: ", TL.pack (linkShorty shawty)
            ]

shortyAintUri :: TL.Text -> TL.Text
shortyAintUri uri =
  TL.concat [ uri
            , " wasn't a url, did you forget http://?"
            ]

shortyFound :: TL.Text -> TL.Text
shortyFound tbs =
  TL.concat ["<a href=\"", tbs, "\">", tbs, "</a>"]

app :: ScottyR ()
app = do
  get "/" $ do
    uri <- param "uri"
    let parsedUri :: Maybe URI
        parsedUri = parseURI (TL.unpack uri)
    case parsedUri of
      Just _  -> do
        shawty <- liftIO shortyGen
        let shorty = BC.pack shawty
            uri' = encodeUtf8 (TL.toStrict uri)
        resp <- saveURI shorty uri'
        html (shortyCreated resp shawty)
      Nothing -> text (shortyAintUri uri)
  get "/:short" $ do
    short <- param "short"
    uri <- getURI short
    case uri of
      Left reply -> text (TL.pack (show reply))
      Right mbBS -> case mbBS of
        Nothing -> text "uri not found"
        Just bs -> html (shortyFound tbs)
          where tbs :: TL.Text
                tbs = TL.fromStrict (decodeUtf8 bs)

main :: IO ()
main = do
  conn <- R.connect R.defaultConnectInfo
  let read_ r = runReaderT r conn
  scottyT 3000 read_ app
