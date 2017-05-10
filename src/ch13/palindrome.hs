module Palindrome where

import Utils ((|>))
import Control.Monad
import System.Exit (exitSuccess)
import Data.Char

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (isPalindrome line1) of
    True  ->
      do putStrLn "It's a palindrome"
         exitSuccess
    False ->
         putStrLn "Not a palindrome"


isPalindrome :: String -> Bool
isPalindrome xs =
  xs
    |> map toLower
    |> filter (flip elem ['a'..'z'])
    |> (\xs -> xs == reverse xs)
