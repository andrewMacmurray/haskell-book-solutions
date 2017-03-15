module Ch15.Optional where

import Data.Monoid
import Ch15.MonoidLaws
import Test.QuickCheck


data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada Nada = Nada
  mappend (Only a) Nada = Only a
  mappend Nada (Only a) = Only a
  mappend (Only a) (Only b) = Only (a <> b)

genOnly :: Arbitrary a => Gen (Optional a)
genOnly = do
  a <- arbitrary
  return (Only a)

genOptional :: (Arbitrary a) => Gen (Optional a)
genOptional =
  frequency [ (1, return Nada)
            , (10, genOnly)
            ]

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = genOptional


newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)


emptyFirst = First' { getFirst' = Nada }

instance Monoid (First' a) where
  mempty = emptyFirst
  mappend (First' { getFirst' = Nada })
          (First' { getFirst' = Nada })   = emptyFirst
  mappend (First' { getFirst' = Nada })
          (First' { getFirst' = Only x }) = First' { getFirst' = Only x }
  mappend (First' { getFirst' = Only x })
          (First' { getFirst' = Nada })   = First' { getFirst' = Only x }
  mappend (First' { getFirst' = Only x })
          (First' { getFirst' = Only _ }) = First' { getFirst' = Only x }


genFirst :: Arbitrary a => Gen (First' a)
genFirst = do
  a <- arbitrary
  return First' { getFirst' = a }

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = genFirst


type FirstMappend = First' String -> First' String -> First' String -> Bool
type FirstId = First' String -> Bool


main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FirstId)
  quickCheck (monoidRightIdentity :: FirstId)
