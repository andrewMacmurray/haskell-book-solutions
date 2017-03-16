module Ch15.Semigroup where

import Data.Semigroup
import Test.QuickCheck


data Trivial =
  Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc =
  Trivial ->
  Trivial ->
  Trivial ->
  Bool



newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

type IdentityAssoc =
  Identity String ->
  Identity String ->
  Identity String ->
  Bool



data Two a b =
  Two a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

type TwoAssoc =
  Two [Sum Int] String ->
  Two [Sum Int] String ->
  Two [Sum Int] String ->
  Bool



data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance
  ( Semigroup a
  , Semigroup b
  , Semigroup c ) => Semigroup (Three a b c) where
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (Three a b c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = threeGen

type ThreeAssoc =
  Three String String String ->
  Three String String String ->
  Three String String String ->
  Bool



data Four a b c d =
  Four a b c d
  deriving (Eq, Show)

instance
  ( Semigroup a
  , Semigroup b
  , Semigroup c
  , Semigroup d ) => Semigroup (Four a b c d) where
  (Four a b c d) <> (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
fourGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return (Four a b c d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = fourGen

type FourAssoc =
  Four String String String (Product Rational) ->
  Four String String String (Product Rational) ->
  Four String String String (Product Rational) ->
  Bool



newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  (BoolConj True) <> (BoolConj _)    = BoolConj False
  (BoolConj False) <> (BoolConj _)   = BoolConj False


boolConjGen :: Gen BoolConj
boolConjGen =
  elements [ BoolConj False, BoolConj True ]

instance Arbitrary BoolConj where
  arbitrary = boolConjGen

type BoolConjAssoc =
  BoolConj ->
  BoolConj ->
  BoolConj ->
  Bool




newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  (BoolDisj False) <> (BoolDisj _)     = BoolDisj True
  (BoolDisj True) <> (BoolDisj _)      = BoolDisj True

boolDisjGen :: Gen BoolDisj
boolDisjGen =
  elements [ BoolDisj False, BoolDisj True ]

instance Arbitrary BoolDisj where
  arbitrary = boolDisjGen

type BoolDisjAssoc =
  BoolDisj ->
  BoolDisj ->
  BoolDisj ->
  Bool



data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  Snd a <> _     = Snd a
  Fst _ <> Snd a = Snd a
  Fst a <> _     = Fst a

orGen :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
orGen = do
  a <- arbitrary
  b <- arbitrary
  elements [ Fst a, Snd b ]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = orGen

type OrAssoc =
  Or String String ->
  Or String String ->
  Or String String ->
  Bool



newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (f <> g)



newtype Comp a =
  Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
  Comp f <> Comp g = Comp (g . f)


genFunc :: (CoArbitrary a, Arbitrary a) => Gen (a -> a)
genFunc = arbitrary

genComp :: (CoArbitrary a, Arbitrary a) => Gen (Comp a)
genComp = do
  f <- genFunc
  return (Comp { unComp = f })




data Validation a b =
    Failure' a
  | Success' b
  deriving (Eq, Show)

instance Semigroup (Validation a b) where
  (Success' a) <> (Success' _) = Success' a
  (Success' _) <> (Failure' a) = Failure' a
  (Failure' a) <> _            = Failure' a

validationGen :: (Arbitrary a, Arbitrary b) => Gen (Validation a b)
validationGen = do
  a <- arbitrary
  b <- arbitrary
  elements [ Failure' a, Success' b ]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = validationGen

type ValidationAssoc =
  Validation String Int ->
  Validation String Int ->
  Validation String Int ->
  Bool



newtype AccumulateRight a b =
  AccumulateRight (Validation a b)
  deriving (Eq, Show)


instance Semigroup b => Semigroup (AccumulateRight a b) where
  (<>) (AccumulateRight (Success' b))
       (AccumulateRight (Success' b'))  = AccumulateRight $ Success' (b <> b')
  (<>) (AccumulateRight (Success' _))
       (AccumulateRight (Failure' a))   = AccumulateRight $ Failure' a
  (<>) (AccumulateRight (Failure' a)) _ = AccumulateRight $ Failure' a

accumRightGen :: (Arbitrary a, Arbitrary b) => Gen (AccumulateRight a b)
accumRightGen = do
  a <- validationGen
  b <- validationGen
  frequency [ (10, return $ AccumulateRight a )
            , (1,  return $ AccumulateRight b)
            ]

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = accumRightGen

type AccumRightAssoc =
  AccumulateRight String (Sum Int) ->
  AccumulateRight String (Sum Int) ->
  AccumulateRight String (Sum Int) ->
  Bool


newtype AccumulateBoth a b =
  AccumulateBoth (Validation a b)
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  (<>) (AccumulateBoth (Success' a))
       (AccumulateBoth (Success' b)) = AccumulateBoth $ Success' (a <> b)
  (<>) (AccumulateBoth (Success' _))
       (AccumulateBoth (Failure' a)) = AccumulateBoth $ Failure' a
  (<>) (AccumulateBoth (Failure' a))
       (AccumulateBoth (Success' _)) = AccumulateBoth $ Failure' a
  (<>) (AccumulateBoth (Failure' a))
       (AccumulateBoth (Failure' b)) = AccumulateBoth $ Failure' (a <> b)


accumBothGen :: (Arbitrary a, Arbitrary b) => Gen (AccumulateBoth a b)
accumBothGen = do
  a <- arbitrary
  b <- arbitrary
  elements [ AccumulateBoth a, AccumulateBoth b ]

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = accumBothGen


type AccumBothAssoc =
  AccumulateBoth String (Product Int) ->
  AccumulateBoth String (Product Int) ->
  AccumulateBoth String (Product Int) ->
  Bool


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a c b = (a <> (b <> c)) == ((a <> b) <> c)

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  quickCheck (semigroupAssoc :: ValidationAssoc)
  quickCheck (semigroupAssoc :: AccumRightAssoc)
  quickCheck (semigroupAssoc :: AccumBothAssoc)
