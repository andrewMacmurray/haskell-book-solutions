module Ch17.Cow where

import Control.Applicative

data Cow = Cow
  { name :: String
  , age :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty s = Just s

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0    = Just n
  | otherwise = Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString n a w =
  Cow <$> noEmpty n
      <*> noNegative a
      <*> noNegative w

cowFromString' :: String -> Int -> Int -> Maybe Cow
cowFromString' n a w =
  liftA3 Cow (noEmpty n)
             (noNegative a)
             (noNegative w)


-- With Maybe Monad

cowFromString'' :: String -> Int -> Int -> Maybe Cow
cowFromString'' n a w =
  noEmpty n >>=
  \name ->
    noNegative a >>=
  \age ->
    noNegative w >>=
  \weight ->
    return $ Cow name age weight

cowFromString''' :: String -> Int -> Int -> Maybe Cow
cowFromString''' n a w = do
  name <- noEmpty n
  age <- noNegative a
  weight <- noNegative w
  return $ Cow n a w


-- fixer upper
x = const <$> Just "hello" <*> Just "world"
y = (,,,) <$> Just 90
          <*> Just 10
          <*> Just "Tierness"
          <*> Just [1..3]
