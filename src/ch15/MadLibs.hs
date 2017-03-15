module Ch15.MadLibs where

import Data.Monoid

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlib :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlib e adv noun adj =
  e <> "! he said " <>
  adv <> " as he jumped into his car " <>
  noun <> " and drove off with his " <>
  adj <> " wife."


madlibBetter :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibBetter e adv noun adj =
  mconcat
    [ e, "! he said "
    , adv, " as he jumped into his car "
    , noun, " and drove off with his "
    , adj, " wife."
    ]
