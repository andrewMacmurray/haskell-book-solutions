module Ch14.Addition where

import Test.Hspec
import Test.QuickCheck
import Ch8.Recur
import Ch14.Generators

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      2 + 2 `shouldBe` 4
    it "x + 1 is always greater than x" $ do
      quickCheck $ forAll randomToOneHundred (\x -> x + 1 > (x :: Int))

  describe "dividedBy" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (Result 5)
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (Result 4)

  describe "multRecur" $ do
    it "multiplies 2 by 2 correctly" $ do
      multRecur 2 2 `shouldBe` 4
    it "multiplies 98 by 67" $ do
      multRecur 98 67 `shouldBe` 6566
