module Main where

import Ch11.Vigenere (vigenere, unvigenere, Direction(..))
import System.Environment
import System.IO
import System.Exit

-- executable program
-- where user can specify either -d or -e to decrypt or encrypt a message

main :: IO ()
main = do
  options <- getArgs
  let dir = direction options
  either handleInvalid handleDirection dir

handleInvalid :: String -> IO ()
handleInvalid err =
  hPutStr stderr $ newline err

handleDirection :: Direction -> IO ()
handleDirection direction = do
  (message, key) <- getPhraseAndKey direction
  putStrLn . successMessage $ direction
  putMessage direction message key
  exitSuccess

putMessage :: Direction -> String -> String -> IO ()
putMessage Forwards message key  = putMessage' vigenere message key
putMessage Backwards message key = putMessage' unvigenere message key

putMessage' :: (String -> String -> String) -> String -> String -> IO ()
putMessage' fn m k = hPutStr stdout . newline $ fn m k

getPhraseAndKey :: Direction -> IO (String, String)
getPhraseAndKey direction = do
  putStrLn $ "enter your message to " ++ (show direction) ++ ": "
  message <- getLine
  putStrLn "enter key phrase: "
  key     <- getLine
  return (message, key)

instance Show Direction where
  show Forwards  = "encrypt"
  show Backwards = "decrypt"

direction :: [String] -> Either String Direction
direction []     = Left directionMessage
direction (x:xs) =
  case x of
    "-d" -> Right Backwards
    "-e" -> Right Forwards
    _    -> Left  directionMessage'

successMessage :: Direction -> String
successMessage Forwards  = "Here is your encrypted message"
successMessage Backwards = "Here is your decrypted message"

directionMessage :: String
directionMessage = "Please specify a direction either -d or -e"

directionMessage' :: String
directionMessage' = "Invalid direction, please specifiy either -d or -e"

newline :: String -> String
newline x = x `mappend` "\n"
