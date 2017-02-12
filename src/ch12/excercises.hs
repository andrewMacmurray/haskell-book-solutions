module Excercises where

-- id :: a -> a
-- a has the kind *

-- r :: a -> f a
-- a has the kind *
-- f has the kind * -> *

notThe :: String -> Maybe String
notThe ('t':'h':'e':' ':xs) = Nothing
notThe xs                   = Just xs

replaceThe :: String -> String
replaceThe "" = ""
replaceThe xs =
  case notThe xs of
    Nothing     -> 'a' : replaceThe (drop 3 xs)
    Just (y:ys) -> y   : replaceThe ys

isVowel :: Char -> Bool
isVowel x = any (== x) "aeiou"

beforeVowel phrase@('t':'h':'e':' ':x:xs)
  | isVowel x  = Nothing
  | otherwise  = Just phrase
beforeVowel xs = Just xs

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel "" = 0
countTheBeforeVowel xs =
  case beforeVowel xs of
    Nothing     -> 1 + countTheBeforeVowel (drop 4 xs)
    Just (y:ys) -> 0 + countTheBeforeVowel ys

x |> f = f x

countInWord :: (Char -> Bool) -> String -> Integer
countInWord f xs =
  xs
    |> filter f
    |> length
    |> toInteger

countVowels :: String -> Integer
countVowels = countInWord isVowel

isConsonant :: Char -> Bool
isConsonant x = any (== x) consonants
  where
    consonants = filter (not . isVowel) ['a'..'z']

countConsonants :: String -> Integer
countConsonants = countInWord isConsonant

newtype Word' =
  Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord xs
  | invalidWord = Nothing
  | otherwise   = Just (Word' xs)
  where
    invalidWord = countVowels xs > countConsonants xs
