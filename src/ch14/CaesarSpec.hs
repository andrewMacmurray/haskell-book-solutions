module Ch14.CaesarSpec where

import Data.Char
import Test.QuickCheck
import Ch9.Caesar

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
