module Ch24.IP6Address where

import Control.Applicative
import Data.Word
import Numeric (readHex)
import Text.Trifecta
import Utils ((|>))

data IP6Address =
  IP6Address Word64
  deriving (Eq, Ord, Show)


ipEx = "0:0:0:0:0:ffff:ac10:fe01"
ipEx' = "0:0:0:0:0:ffff:cc78:f"
ipEx'' = "FE80:0000:0000:0000:0202:B3FF:FE1E:8329"
ipEx''' = "2001:DB8::8:800:200C:417A"


-- a bit baffed by the maths, wrong answer
parseIps :: Parser IP6Address
parseIps = do
  xs <- parseHexes
  return $ IP6Address (sumHexes xs)


sumHexes :: [String] -> Word64
sumHexes xs =
  xs
  |> map readHex
  |> concat
  |> map fst
  |> sum

parseHexes :: Parser [String]
parseHexes =
  hexes `sepBy` colons


colons :: Parser String
colons = do
  try (symbol "::") <|> try (symbol ":")

hexes :: Parser String
hexes = many hexDigit


main :: IO ()
main = do
  let printAddresses = print . parseString parseIps mempty
      addresses = [ ipEx
                  , ipEx'
                  , ipEx''
                  , ipEx'''
                  ]
  mapM_ printAddresses addresses
