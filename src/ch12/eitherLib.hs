module EitherLib where

lefts' :: [Either a b] -> [a]
lefts' = foldr concatLeft []
  where
    concatLeft (Left a) xs  = a : xs
    concatLeft (Right _) xs = xs

rights' :: [Either a b] -> [b]
rights' = foldr concatRight []
  where
    concatRight (Left _) xs  = xs
    concatRight (Right a) xs = a : xs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _)  = Nothing
eitherMaybe' f (Right x) = Just (f x)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x)  = f x
either' _ g (Right y) = g y

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f et = either' (\_ -> Nothing) (Just . f) et
