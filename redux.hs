module Main where

data Expr = Abstract String Expr | Var String | Apply Expr Expr
instance Show Expr where
  show (Var x) = x
  show (Abstract x y) = "λ" ++ x ++ "." ++ (show y)
  show (Apply x y) = "(" ++ (show x) ++ ")" ++ (show y)
instance Eq Expr where
  (Var x) == (Var y) = x == y
  (Abstract a x) == (Abstract b y) = x == (subst y b a)
  (Apply a b) == (Apply x y) = x == y

subst :: Expr -> String -> String -> Expr
subst (Var x) a b = if x == a then Var b else Var x
subst (Abstract x y) a b = if x == a then (Abstract x y) else (Abstract x (subst y a b))
subst (Apply x y) a b = Apply (subst x a b) (subst y a b)

main = putStrLn . show $ (Abstract "x" (Var "x")) == (Abstract "y" (Var "y"))

