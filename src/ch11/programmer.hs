module Programmer where

data OperatingSystem =
    GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgrammingLanguage =
    Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer
    { os :: OperatingSystem
    , lang :: ProgrammingLanguage
    }
  deriving (Eq, Show)

allOs :: [OperatingSystem]
allOs =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgrammingLanguage]
allLanguages =
  [ Haskell
  , Agda
  , Idris
  , PureScript
  ]

-- creates a list with all possible combinations of os and language
-- 16 combinations in total
allProgammers :: [Programmer]
allProgammers =
  construct allOs allLanguages
  where
    construct [] _ = []
    construct (x:xs) ys =
      map (Programmer x) ys ++ construct xs ys
