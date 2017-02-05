module MyMap where
import Data.Bool

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

vowels :: String -> [Bool]
vowels = map (\x -> elem x "aeiou")

foldBool :: (Num b, Eq b) => [b] -> [b]
foldBool xs = map (\x -> bool x (-x) $ x == 3) xs
