module Ch24.Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)


virtuousParseFraction :: Parser Rational
virtuousParseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "denominator cannot be zero"
    _ -> return (numerator % denominator)


parseThreeDigits :: Parser Integer
parseThreeDigits = do
  a <- integer
  let parseResult
        | a > 999   = fail "over 3 digits"
        | a < -999  = fail "over 3 digits"
        | otherwise = return a
  parseResult


type NumberOrFraction =
  Either Double Rational


parseNumberOrFraction :: Parser NumberOrFraction
parseNumberOrFraction =
  (Left <$> try double) <|> (Right <$> try virtuousParseFraction)


main :: IO ()
main = do
  print $ parseString parseThreeDigits mempty "123"
  print $ parseString parseThreeDigits mempty "1234"
  print $ parseString parseNumberOrFraction mempty shouldWork
  print $ parseString parseNumberOrFraction mempty "1.5"
