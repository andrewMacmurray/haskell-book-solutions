module Cipher.Caesar where

import Data.Char

lowerBase = 97
upperBase = 65

-- caesar 1

toCharBase :: Int -> Int
toCharBase x = if x > 90
  then x - lowerBase
  else x - upperBase

applyOffset :: Int -> Int -> Int
applyOffset shift x =
  mod (x + shift) 26

shift n =
    chr
  . (+) lowerBase
  . applyOffset n
  . toCharBase
  . ord

caesar :: Int -> String -> String
caesar offset = map (shift offset)



-- caesar 2

data CharCase =
    Upper
  | Lower
  deriving (Show, Eq)

checkCase x = if isUpper x
  then (Upper, ord x)
  else (Lower, ord x)

toCharBase2 (chr, x) = if chr == Upper
  then (chr, x - upperBase)
  else (chr, x - lowerBase)

applyOffset2 shift (chr, x) =
  (chr, mod (x + shift) 26)

fromCharBase (chr, x) = if chr == Upper
  then x + upperBase
  else x + lowerBase

caesar2 :: Int -> String -> String
caesar2 offset =
  map (
      chr
    . fromCharBase
    . applyOffset2 offset
    . toCharBase2
    . checkCase
  )



-- uncaesar

uncaesar :: Int -> String -> String
uncaesar offset = map (shift $ negate offset)
