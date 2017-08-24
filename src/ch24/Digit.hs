module Ch24.Digit where

import Control.Applicative
import Text.Trifecta

parseDigit :: Parser Char
parseDigit = oneOf ['0'..'9'] <?> "whoops parseDigit messed up"


base10Integer :: Parser Integer
base10Integer = do
  d <- some parseDigit
  return $ read d

base10Integer' :: Parser Integer
base10Integer' = some parseDigit >>= (return . read)

base10Neg :: Parser Integer
base10Neg = do
  _ <- char '-'
  n <- base10Integer
  return $ negate n

base10HandleBoth :: Parser Integer
base10HandleBoth = do
  (try base10Integer) <|> (try base10Neg)

main :: IO ()
main = do
  print $ parseString parseDigit mempty "123"
  print $ parseString parseDigit mempty "abc"
  print $ parseString base10Integer mempty "123abc"
  print $ parseString base10Integer mempty "abc"
  print $ parseString base10HandleBoth mempty "-123abc"
