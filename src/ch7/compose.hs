module Compose where

oddsFrom :: Integer -> [Integer]
oddsFrom = take 5 . filter odd . enumFrom

negateSum :: Num a => [a] -> a
negateSum = negate . sum

mySum :: Num a => [a] -> a
mySum = foldl (+) 0

ay :: [Char] -> Int
ay = length . filter (== 'a')
