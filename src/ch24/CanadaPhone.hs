module Ch24.CanadaPhone where

import Control.Applicative
import Text.Trifecta

type NumberingPlanArea = Integer
type Exchange = Integer
type LineNumber = Integer

data PhoneNumber =
  PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)


parsePhone :: Parser PhoneNumber
parsePhone = do
  try allInOne <|> parsePhone'


parsePhone' :: Parser PhoneNumber
parsePhone' = do
  a <- planArea
  _ <- phoneSpaces
  b <- threeDigits
  _ <- phoneSpaces
  c <- fourDigits
  return $ PhoneNumber a b c

allInOne :: Parser PhoneNumber
allInOne = do
  xs <- some digit
  let a = take 3 xs
      b = take 3 . drop 3 $ xs
      c = drop 6 xs
      correctLength = length xs == 10
      result
        | correctLength = return $ PhoneNumber (read a) (read b) (read c)
        | otherwise     = fail "incorrect length"
  result


phoneSpaces :: Parser Char
phoneSpaces = oneOf [' ', '-']

planArea :: Parser Integer
planArea = do
      try prefixedPlanArea
  <|> try bracketPlanArea
  <|> try threeDigits


bracketPlanArea :: Parser Integer
bracketPlanArea = char '(' *> threeDigits <* char ')'

prefixedPlanArea :: Parser Integer
prefixedPlanArea = string "1-" >> threeDigits

threeDigits :: Parser Integer
threeDigits = digitRange 999 100

fourDigits :: Parser Integer
fourDigits = digitRange 9999 1000

digitRange :: Integer -> Integer -> Parser Integer
digitRange max min = do
  n <- integer
  let result
        | n > max   = fail "digit above max"
        | n < min   = fail "digit below min"
        | otherwise = return n
  result

main :: IO ()
main = do
  print $ parseString parsePhone mempty "123-456-7890"
  print $ parseString parsePhone mempty "1234567890"
  print $ parseString parsePhone mempty "(123) 456-7890"
  print $ parseString parsePhone mempty "1-123-456-7890"
