module Cardinality where
import Data.Int

-- Sum types

data BigSmall =
    Big Bool
  | Small Bool
  deriving (Eq, Show)

-- BigSmall has 4 possible values:
-- Big True, Big False, Small True, Small False
-- cardinality of 4

data NumberOrBool =
    Numba Int8
  | BoolyBool Bool
  deriving (Eq, Show)

-- Int8 has 256 possible values (-128..127)
-- Bool has 2 possible values
-- cardinality of 256 + 2 = 258



-- Product types

data QuantumBool =
    QuantumTrue
  | QuantumFalse
  | QuantumBoth
  deriving (Eq, Show)

-- cardinality of 3 (still sum type)

-- a product type
data TwoQs =
  MkTwoQs QuantumBool QuantumBool
  deriving (Eq, Show)

-- TwoQs can have 3 * 3 possible values
-- cardinality of 9

-- same as above but called an anonymous product type
-- all tuples are anonymous products
type TwoQs2 = (QuantumBool, QuantumBool)
