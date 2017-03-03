module Spec.Caesar where

import Data.Char
import Test.QuickCheck

isLowerCase x = any (== x) ['a'..'z']
onlyLowercase = filter isLowerCase

caesarGen :: Gen (Int, String)
caesarGen = do
  a <- arbitrary
  b <- arbitrary
  let c = onlyLowercase b
  return (a, c)

prop_caesar =
  forAll caesarGen
  (\(x, y) -> (uncaesar x (caesar x y)) == y)

main :: IO ()
main = do
  putStrLn "checking caesar"
  quickCheck prop_caesar




-- caesar

lowerBase = 97
upperBase = 65


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

uncaesar :: Int -> String -> String
uncaesar offset = map (shift $ negate offset)
