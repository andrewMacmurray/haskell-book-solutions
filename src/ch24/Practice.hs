module Ch24.Practice where

import Text.Trifecta

one :: Parser Char
one = char '1'

oneTwo :: Parser Char
oneTwo = one >> char '2'

oneTwoStop :: Parser ()
oneTwoStop = oneTwo >> eof

allThree :: Parser String
allThree = do
  n <- (show <$> integer)
  _ <- eof
  return n


string' :: String -> Parser String
string' str = go str mempty
  where
    go (x:xs) parsed = char x >>= (\x' -> go xs (parsed ++ [x']))
    go [] parsed     = return parsed


main :: IO ()
main = do
  print $ parseString oneTwo mempty "12"
  print $ parseString oneTwoStop mempty "12"
  print $ parseString oneTwoStop mempty "123"
  print $ parseString allThree mempty "12"
  print $ parseString allThree mempty "123"
  print $ parseString (string' "hello") mempty "hello"
