module Exercises where
import Data.Char

capitalise :: String -> String
capitalise (x:xs) = toUpper x : xs

uppercase :: String -> String
uppercase [] = []
uppercase (x:xs) = toUpper x : uppercase xs

firstCapitalise :: String -> Char
firstCapitalise = toUpper . head
