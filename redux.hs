module Main where

data Expr = Abstract String Expr | Var String
instance Show Expr where
  show (Var x) = x
  show (Abstract x y) = "λ" ++ x ++ "." ++ (show y)
main = putStrLn . show $ Abstract "x" (Var "x")

