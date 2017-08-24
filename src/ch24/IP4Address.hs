module Ch24.IP4Address where

import Text.Trifecta
import Data.Word
import Ch24.CanadaPhone (threeDigits, digitRange)

data IP4Address =
  IP4Address Word32
  deriving (Eq, Ord, Show)


ipEx = "172.16.254.1"
ipEx' = "204.120.0.15"


ipAddress :: Parser IP4Address
ipAddress = do
  a <- threeDigits
  _ <- char '.'
  b <- digitRange 999 10
  _ <- char '.'
  c <- digitRange 999 0
  _ <- char '.'
  d <- digitRange 99 0
  return $ IP4Address . fromIntegral $ a * 256^3 + b * 256^2 + c * 256 + d


main :: IO ()
main = do
  print $ parseString ipAddress mempty ipEx
  print $ parseString ipAddress mempty ipEx'
