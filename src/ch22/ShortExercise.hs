module Ch22.ShortExercise where

import Data.Char
import Control.Applicative

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> rev <*> cap

tupled' :: [Char] -> ([Char], [Char])
tupled' = liftA2 (,) rev cap

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = do
  r <- rev
  c <- cap
  return (r, c)

tupled''' :: [Char] -> ([Char], [Char])
tupled''' =
  rev >>= (\r -> cap >>= (\c -> return (r, c)))
