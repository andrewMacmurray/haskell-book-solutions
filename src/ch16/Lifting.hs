module Ch16.Lifting where

a :: [Int]
a = (+1) <$> read "[1]"

b :: Maybe [String]
b = (fmap . fmap) (++ "lol") $ Just ["Hi,", "Hello"]

c :: Int -> Int
c = (*2) . (\x -> x - 2)

d :: Int -> String
d = ((return '1' ++) . show) <$> (\x -> [x, 1..3])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = read <$> ("123" ++) <$> show <$> ioi
    in
      (*3) <$> changed
