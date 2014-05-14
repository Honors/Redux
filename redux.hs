module Main where

data Expr = Abstract String Expr | Var String | Apply Expr Expr
instance Show Expr where
  show (Var x) = x
  show (Abstract x y) = "λ" ++ x ++ "." ++ (show y)
  show (Apply x y) = "(" ++ (show x) ++ ")" ++ (show y)
main = putStrLn . show $ Apply (Abstract "x" (Var "x")) (Abstract "x" (Var "x"))

