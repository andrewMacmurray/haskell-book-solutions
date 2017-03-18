module Ch15.Monoid where

import Test.QuickCheck
import Test.Hspec
import Ch15.Semigroup
import Ch15.MonoidLaws
import Data.Semigroup

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)


instance (Monoid a, Semigroup a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)


instance ( Monoid a
         , Semigroup a
         , Monoid b
         , Semigroup b ) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)


instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

instance (Monoid b, Semigroup b) => Monoid (Combine a b) where
  mempty = Combine mempty
  mappend = (<>)

instance (Monoid a, Semigroup a) => Monoid (Comp a) where
  mempty = Comp mempty
  mappend = (<>)


newtype Mem s a =
  Mem { runMem :: s -> (a, s) }

combineMems f g x =
  let
    (a, b) = g x
    (c, d) = f b
  in
    (a <> c, d)

instance (Semigroup a) => Semigroup (Mem s a) where
  (<>) (Mem { runMem = f })
       (Mem { runMem = g }) = Mem $ combineMems f g

instance (Monoid a, Semigroup a) => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend = (<>)


mSpec :: IO ()
mSpec = do
  putStrLn "Trivial monoid"
  quickCheck (monoidAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)

  putStrLn "Identity monoid"
  quickCheck (monoidAssoc :: IdentityAssoc)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)

  putStrLn "Two monoid"
  quickCheck (monoidAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two String String -> Bool)
  quickCheck (monoidRightIdentity :: Two String String -> Bool)

  putStrLn "BoolConj monoid"
  quickCheck (monoidAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)

  putStrLn "BoolDisj monoid"
  quickCheck (monoidAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

  hspec $ do
    it "Mem combines functions correctly" $ do
      let f' = Mem $ \x -> ("Hi", x + 1)

      runMem (f' <> mempty) 0 `shouldBe` ("Hi", 1)
      runMem (mempty <> f') 0 `shouldBe` ("Hi", 1)
      (runMem mempty 0 :: (String, Int)) `shouldBe` ("", 0)
      runMem (f' <> mempty) 0 == runMem f' 0 `shouldBe` True
      runMem (mempty <> f') 0 == runMem f' 0 `shouldBe` True
