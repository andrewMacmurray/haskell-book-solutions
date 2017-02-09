module As where

import Data.Char

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) phrase@(y:ys) = if x `elem` phrase
  then isSubsequenceOf xs ys
  else isSubsequenceOf (x:xs) ys


makePair :: String -> (String, String)
makePair xs = (xs, capitalizeWord xs)

capitalizedWords :: String -> [(String, String)]
capitalizedWords =
  map makePair . words

capitalizeWord :: String -> String
capitalizeWord (x:xs) = (toUpper x) : xs


capitalizeParagraph :: String -> String
capitalizeParagraph para = go (capitalizeWord para)
  where
    go [] = []
    go ('.':' ':xs) = ". " ++ (go $ capitalizeWord xs)
    go (x:xs) = x : go xs
