{-# LANGUAGE FlexibleInstances #-}

module Ch16.Exercises where

data Sum a b =
    First b
  | Second a
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First b) = First (f b)
  fmap f (Second a) = Second a



data Company a b c =
    DeepBlue a b
  | Something c
  deriving (Eq, Show)

instance Functor (Company a b) where
  fmap f (Something c) = Something (f c)
  fmap _ (DeepBlue a b) = DeepBlue a b



data More a b =
    L b a b
  | R a b a
  deriving (Eq, Show)

instance Functor (More a) where
  fmap f (L a b c) = L (f a) b (f c)
  fmap f (R a b c) = R a (f b) c



data Quant a b =
    Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk a)  = Desk a
  fmap f (Bloor b) = Bloor (f b)

data K a b =
  K a
  deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K a) = K a



newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

newtype K' a b =
  K' a

instance Functor (Flip K' a) where
  fmap f (Flip (K' x)) = Flip $ K' (f x)



data EvilGoateeConst a b =
  GoatyConst b
  deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)



data LiftItOut f a =
  LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut ga) = LiftItOut (fmap f ga)



data Parappa f g a =
  DaWrappa (f a) (g a)

instance (Functor f, Functor f1) => Functor (Parappa f f1) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)



data IgnoreOne f g a b =
  IgnoreSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoreSomething fa gb) = IgnoreSomething fa (fmap f gb)



data Notorious g o a t =
  Notorious (g o) (g a) (g t)

instance Functor f => Functor (Notorious f o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)



data List a =
    Nil
  | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a rest) = Cons (f a) (fmap f rest)



data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)



data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print x a) = Print x (f a)
  fmap f (Read sa) = Read (f . sa)
