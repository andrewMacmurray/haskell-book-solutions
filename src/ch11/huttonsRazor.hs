module Razor where

data Expr
  = Lit Integer
  | Add Expr Expr

exampleExpr :: Expr
exampleExpr = Add (Lit 1) (Lit 9001)

eval :: Expr -> Integer
eval (Lit n) = n
eval (Add expr1 expr2) = eval expr1 + eval expr2

printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add expr1 expr2) = printExpr expr1 ++ " + " ++ printExpr expr2
