module Utils where

-- forwards pipe operator (like in elm)
(|>) :: a -> (a -> b) -> b
x |> f = f x
