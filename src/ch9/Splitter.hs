module Splitter where

myWords :: String -> [String]
myWords "" = []
myWords xs =
  let
    notEmpty = (/=) ' '
    word = takeWhile notEmpty xs
    rest = dropWhile notEmpty xs
    next = if rest == [] then [] else tail rest
  in
    word : myWords next

str = "hello world hello"


firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences =
       firstSen
    ++ secondSen
    ++ thirdSen
    ++ fourthSen


myLines :: String -> [String]
myLines "" = []
myLines xs =
  let
    notNewLine = (/=) '\n'
    line = takeWhile notNewLine xs
    rest = dropWhile notNewLine xs
    next = if rest == [] then [] else tail rest
  in
    line : myLines next


splitOn :: Char -> String -> [String]
splitOn char xs =
  let
    notChar = (/=) char
    first = takeWhile notChar xs
    rest = dropWhile notChar xs
    next = if rest == [] then [] else splitOn char (tail rest)
  in
    first : next
