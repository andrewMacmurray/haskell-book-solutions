module TupleFunctions where

addUp :: Num a => (a, a) -> a
addUp (x, y) = x + y

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
