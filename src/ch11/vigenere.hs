module Ch11.Vigenere where

import Ch9.Caesar
import Data.Char
import Data.List


-- prompts the user to enter a message and keyword
-- shows the encoded message on screen

main :: IO ()
main = do
  putStrLn "enter your message: "
  message <- getLine
  putStrLn "enter a key phrase: "
  key     <- getLine
  putStrLn $ "Your encrypted message is: " ++ (vignere message key)


-- keyword  = "ALLY"
-- message  = "MEET AT DAWN"

-- key      = "ALLY AL LYAL"
-- encoded  = "MPPR AE OYWY"

data Direction =
    Forwards
  | Backwards
  deriving (Eq)


unvignere :: String -> String -> String
unvignere = vig Backwards

vignere :: String -> String -> String
vignere = vig Forwards

vig :: Direction -> String -> String -> String
vig _ "" _ = ""
vig _ x "" = x
vig direction body keyword = encoded
  where
    key = makeKey body keyword
    encoded = zipWith (shiftChar direction) body key


makeKey :: String -> String -> String
makeKey message keyword = go message keyCycle
  where
    keyCycle = cycle keyword
    go [] _ = []
    go (x:xs) (y:ys)
      | x == ' '  = ' ' : go xs (y:ys)
      | otherwise =   y : go xs ys


shiftChar :: Direction -> Char -> Char -> Char
shiftChar _ ' ' _ = ' '
shiftChar direction char key = shift convert char
  where
    f = toCharBase . ord $ key
    convert = case direction of
      Forwards -> f
      Backwards -> negate f
