module Phone where

import Data.Char
import Data.List

data Button = Button Digit String deriving (Show)
type Digit = Char
type Presses = Int

data Phone = Phone [Button] deriving (Show)

phone :: Phone
phone = Phone
  [ Button '1' "1"
  , Button '2' "abc2"
  , Button '3' "def3"
  , Button '4' "ghi4"
  , Button '5' "jkl5"
  , Button '6' "mno6"
  , Button '7' "pqrs7"
  , Button '8' "tuv8"
  , Button '9' "wxyz9"
  , Button '*' ""
  , Button '0' " 0"
  , Button '#' "."
  ]

convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol lol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Haha thanks just making sure rofl ur turn"
  ]


-- forwards pipe operator (like in elm)
(|>) :: a -> (a -> b) -> b
x |> f = f x

containsCharacter :: Char -> Button -> Bool
containsCharacter x (Button y ys) = (toLower x) `elem` ys

findButton :: Phone -> Char -> Button
findButton (Phone buttons) x =
  buttons
    |> filter (containsCharacter x)
    |> head

numberOfPresses :: String -> Char -> Presses
numberOfPresses [] _ = 0
numberOfPresses (x:xs) y = if x == y
  then 1
  else 1 + numberOfPresses xs y

reverseTaps :: Phone -> Char -> [(Digit, Presses)]
reverseTaps phone x = uppercaseTap ++ digitTap
  where
    (Button digit chars) = findButton phone x
    uppercaseTap = if isUpper x then [('*', 1)] else []
    digitTap = [(digit, numberOfPresses chars x)]

flatten :: [[a]] -> [a]
flatten = foldl (++) []

tapsInSentence :: Phone -> String -> [(Digit, Presses)]
tapsInSentence phone =
  flatten . map (reverseTaps phone)

conversationTaps :: [String] -> [(Digit, Presses)]
conversationTaps xs = xs
    |> map (tapsInSentence phone)
    |> flatten

fingertaps :: [(Digit, Presses)] -> Presses
fingertaps = foldr (\(_, n) x -> x + n) 0

baseCharCount :: [(Char, Presses)]
baseCharCount = [ (x, 0) | x <- validCharacters ]
  where
    validCharacters = ['a'..'z'] ++ ['0'..'9'] ++ "."

addToCharCount x (y, n) =
  if toLower x == y
  then (y, n + 1)
  else (y, n)

countSingleChar :: (Char, Presses) -> String -> (Char, Presses)
countSingleChar = foldr addToCharCount

sentenceCharCount xs =
  map (\x -> countSingleChar x xs) baseCharCount

popularLetter :: String -> Char
popularLetter xs =
  xs
    |> sentenceCharCount
    |> maximumBy (\(x, a) (y, b) -> compare a b)
    |> (\(c, n) -> c)

overallPopularLetter :: [String] -> Char
overallPopularLetter xs =
  xs
    |> foldl (++) ""
    |> mostPopularLetter
