module MaybeLib where

isJust :: Maybe a -> Bool
isJust Nothing  = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing Nothing  = True
isNothing (Just _) = False

maybeWithDefault :: b -> (a -> b) -> Maybe a -> b
maybeWithDefault x f Nothing  = x
maybeWithDefault x f (Just y) = f y

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing  = x
fromMaybe _ (Just y) = y

listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing   = []
maybeToList (Just x)  = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes = concat . map maybeToList

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Nothing
flipMaybe xs
  | any isNothing xs = Nothing
  | otherwise        = Just (catMaybes xs)
