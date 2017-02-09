module Phone where

import Data.Char

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

containsCharacter :: Char -> Button -> Bool
containsCharacter x (Button y ys) = (toLower x) `elem` ys

findButton :: Phone -> Char -> Button
findButton (Phone buttons) x = head . filter (containsCharacter x) $ buttons

numberOfPresses :: String -> Char -> Presses
numberOfPresses [] _ = 0
numberOfPresses (x:xs) y = if x == y
  then 1
  else 1 + numberOfPresses xs y

reverseTaps :: Phone -> Char -> [(Digit, Presses)]
reverseTaps phone x =
  let
    checkUppercase = if isUpper x then [('*', 1)] else []
    (Button digit chars) = findButton phone x
  in
    checkUppercase ++ [(digit, numberOfPresses chars x)]

cellPhonesDead :: Phone -> String -> [(Digit, Presses)]
cellPhonesDead phone =
  foldl (++) [] . map (reverseTaps phone)
