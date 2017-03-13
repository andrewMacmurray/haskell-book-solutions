module Ch14.VignereSpec where

import Test.QuickCheck
import Test.Hspec
import Ch11.Vigenere (vignere, unvignere)

wordGen :: Gen String
wordGen = listOf $ elements ['a'..'z']

keywordMessageGen :: Gen (String, String)
keywordMessageGen = do
  a <- wordGen
  b <- wordGen
  return (a, b)


vignereProp :: Property
vignereProp =
  forAll keywordMessageGen
    (\(message, key) -> (unvignere (vignere message key) key) == message)


main :: IO ()
main = hspec $ do
  describe "vignere cipher" $ do
    it "should encode a string correctly with a message" $ do
      vignere "meet at dawn" "ally" `shouldBe` "mppr ae oywy"

    it "should unencode a string correctly with a key" $ do
      unvignere "mppr ae oywy" "ally" `shouldBe` "meet at dawn"

    it "vignere to unvignere should hold for all values" $ do
      quickCheck vignereProp
