module Spec.Excercises where

import Test.QuickCheck
import Data.List (sort)

-- arithmetic half

half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half

floatGen :: Gen Float
floatGen = arbitrary

prop_half :: Property
prop_half =
  forAll floatGen (\x -> half x == x / 2)

prop_halfIdentity :: Property
prop_halfIdentity =
  forAll floatGen (\x -> x == halfIdentity x)

-- list ordering

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t)  = (Just y, x >= y)


listGen :: Gen [Float]
listGen = arbitrary


prop_sortedList :: Property
prop_sortedList =
  forAll listGen (\xs ->  (listOrdered . sort $ xs) == True)


-- plus

plusAssociative x y z =
  x + (y + z) == (x + y) + z

plusCommutative x y =
  x + y == y + x

threeInts :: Gen (Int, Int, Int)
threeInts = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

twoInts :: Gen (Int, Int)
twoInts = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

prop_add_associative =
  forAll threeInts (\(x, y, z) -> plusAssociative x y z)

prop_add_commutative =
  forAll twoInts (\(x, y) -> plusCommutative x y)

-- mult

multAssociative x y z =
  x * (y * z) == (x * y) * z

multCommutative x y =
  x * y == y * x

prop_mult_associative =
  forAll threeInts (\(x, y, z) -> multAssociative x y z)

prop_mult_commutative =
  forAll twoInts (\(x, y) -> multCommutative x y)

-- quot and rem

nonZeroArbitrary = arbitrary `suchThat` (/= 0)

twoNonZeroes :: Gen (Integer, Integer)
twoNonZeroes = do
  a <- nonZeroArbitrary
  b <- nonZeroArbitrary
  return (a, b)

quotRemEq x y =
  (quot x y) * y + (rem x y) == x

divModEq x y =
  (div x y) * y + (mod x y) == x

prop_quotRemEq =
  forAll twoNonZeroes (\(x, y) -> quotRemEq x y)

prop_divModEq =
  forAll twoNonZeroes (\(x, y) -> divModEq x y)

-- power

nonNegativeArbitrary = arbitrary `suchThat` (> 1)

threeNonNegatives :: Gen (Integer, Integer, Integer)
threeNonNegatives = do
  a <- nonNegativeArbitrary
  b <- nonNegativeArbitrary
  c <- nonNegativeArbitrary
  return (a, b, c)

-- quickCheck verifies this is not true
powerAssociative x y z =
  x ^ (y ^ z) == (x ^ y) ^ z

-- also not true
powerCommutative x y =
  x ^ y == y ^ x

prop_powerAssociative =
  forAll threeNonNegatives (\(x, y, z) -> powerAssociative x y z)

prop_powerCommutative =
  forAll twoNonZeroes (\(x, y) -> powerCommutative x y)

-- reverseId

prop_reverseId =
  forAll (arbitrary :: Gen [String])
  (\x -> (reverse . reverse $ x) == id x)


-- prop $

prop_dollar =
  forAll (arbitrary :: Gen Int)
  (\x -> id $ x == id x)

prop_compose =
  forAll (arbitrary :: Gen Int)
  (\x -> id . id $ x == (id (id x)))

-- fold cons concat

prop_foldCons =
  forAll (arbitrary :: Gen [Int])
  (\x -> (foldr (:) [] x) == ((++) [] x))

prop_foldConcat =
  forAll (arbitrary :: Gen [[Int]])
  (\x -> (foldr (++) [] x) == (concat x))

-- hm is that so

genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

hmm n xs = length (take n xs)

-- proves is not true
prop_hmm =
  forAll (genTuple :: Gen (Int, [Int]))
  (\(x, xs) -> (hmm x xs) == x)

-- round trip
roundTrip x = (read (show x))

prop_roundTrip =
  forAll (arbitrary :: Gen Int)
  (\x -> roundTrip x == x)


main :: IO ()
main = do
  putStrLn "prop half checks"
  quickCheck prop_half
  putStrLn "half identity checks"
  quickCheck prop_halfIdentity
  putStrLn "prop sorted list checks"
  quickCheck prop_sortedList
  putStrLn "plus associative checks"
  quickCheck prop_add_associative
  putStrLn "plus commutative checks"
  quickCheck prop_add_commutative
  putStrLn "mult associative checks"
  quickCheck prop_mult_associative
  putStrLn "mult commutative checks"
  quickCheck prop_mult_commutative
  putStrLn "quot rem checks"
  quickCheck prop_quotRemEq
  putStrLn "div mod checks"
  quickCheck prop_divModEq
  putStrLn "power associative checks"
  quickCheck prop_powerAssociative
  putStrLn "power commutative checks"
  quickCheck prop_powerCommutative
  putStrLn "reverse reverse list"
  quickCheck prop_reverseId
  putStrLn "dollar checks"
  quickCheck prop_dollar
  putStrLn "compose checks"
  quickCheck prop_compose
  putStrLn "prop foldcons"
  quickCheck prop_foldCons
  putStrLn "prop foldConcat"
  quickCheck prop_foldConcat
  putStrLn "prop hmm"
  quickCheck prop_hmm
  putStrLn "round trip check"
  quickCheck prop_roundTrip
