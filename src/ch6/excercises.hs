import Data.List

x :: Int -> Int
x blah = blah + 20

printIt :: IO ()
printIt = putStrLn $ show (x 10)

data Person = Person Bool deriving (Show)

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood =
    Blah
  | Woot
  deriving (Show, Eq)

settleDown :: Mood -> Mood
settleDown x =
  if x == Woot
  then Blah
  else x

type Subject = String
type Verb = String
type Object = String


data Sentence =
  Sentence Subject Verb Object
  deriving (Show, Eq)

-- compiles but cannot print s1 (as no instance of Show for Show (Object -> Sentence))
s1 = Sentence "Dog" "Drool"
s2 = Sentence "Julie" "Loves" "Dogs"



data Rocks =
  Rocks String deriving (Show, Eq)

data Yeah =
  Yeah Bool deriving (Show, Eq)

data Papu =
  Papu Rocks Yeah
  deriving (Show, Eq)

phew = Papu (Rocks "Chases") (Yeah True)

equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'


-- i :: Num a => a
i = 1

f :: RealFrac a => a -> a -> a
f a b =  a + b + 1.0

freud :: Ord a => a -> a
freud x = x


myX = 1 :: Int

sigmund :: Num a => a -> Int
sigmund x = myX

-- jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head $ sort xs

young :: [Char] -> Char
young xs = head $ sort xs

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
-- signifier :: Ord a => [a] -> a
signifier = head . mySort

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aToB a b = (aToB a) == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i a = (fromInteger i) + (f a)
