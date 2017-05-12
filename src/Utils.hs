module Utils where

-- elm style forwards pipe operator
(|>) :: a -> (a -> b) -> b
x |> f = f x
