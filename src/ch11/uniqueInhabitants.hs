module UniqueInhabitants where

data Quad =
    One
  | Two
  | Three
  | Four
  deriving (Eq, Show)


-- product type, has 4 * 4 (16) unique inhabitants
eQuad :: Either Quad Quad
eQuad = undefined

-- same as above but is an anonymous product
prodQuad :: (Quad, Quad)
prodQuad = undefined

-- functions obey exponential laws for cardinality
-- 4 ^ 4 (256)
funcQuad :: Quad -> Quad
funcQuad = undefined

-- 2 * 2 * 2 (8)
prodTBool :: (Bool, Bool, Bool)
prodTBool = undefined

-- 2 ^ (2 * 2) (16)
gTwo :: Bool -> Bool -> Bool
gTwo = undefined

-- 4 ^ (4 * 2) (65536)
fTwo :: Bool -> Quad -> Quad
fTwo = undefined
