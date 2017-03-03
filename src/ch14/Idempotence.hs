module Spec.Idempotence where

import Test.QuickCheck
import Data.Char
import Data.List

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

twice f = f . f
fourTimes = twice . twice

prop_capitalize :: Property
prop_capitalize =
  forAll (arbitrary :: Gen String)
  (\x ->
    (capitalizeWord x == (twice capitalizeWord x)) &&
    (capitalizeWord x == (fourTimes capitalizeWord x)))


prop_sort :: Property
prop_sort =
  forAll (arbitrary :: Gen [Integer])
  (\x ->
    (sort x == (twice sort x)) &&
    (sort x == (fourTimes sort x)))


data Fool =
    Fulse
  | Frue
  deriving (Eq, Show)

foolGen :: Gen Fool
foolGen = elements [Fulse, Frue]

moreFulse :: Gen Fool
moreFulse = frequency [ (3, return Fulse)
                      , (1, return Frue)
                      ]

main :: IO ()
main = do
  putStrLn "capitalizeWord checks"
  quickCheck prop_capitalize
  putStrLn "sort checks"
  quickCheck prop_sort
