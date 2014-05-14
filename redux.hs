module Main where

data Expr = Abstract String Expr | Var String | Apply Expr Expr
instance Show Expr where
  show (Var x) = x
  show (Abstract x y) = "λ" ++ x ++ "." ++ (show y)
  show (Apply x y) = "(" ++ (show x) ++ ")" ++ (show y)
instance Eq Expr where
  (Var x) == (Var y) = x == y
  (Abstract a x) == (Abstract b y) = x == (subst y (Var b) (Var a))
  (Apply a b) == (Apply x y) = x == y
  x == y = False

subst :: Expr -> Expr -> Expr -> Expr
subst (Var x) (Var a) b = if x == a then b else Var x
subst (Var x) a b = Var x
subst (Abstract x y) (Var a) b = if x == a then (Abstract x y) else (Abstract x (subst y (Var a) b))
subst (Abstract x y) a b = Abstract x (subst y a b)
subst (Apply x y) a b = Apply (subst x a b) (subst y a b)

reduce :: Expr -> Expr
reduce (Apply (Abstract x y) a) =
  let subbed = subst y (Var x) a 
  in if subbed == (Apply (Abstract x y) a) then subbed else reduce subbed
reduce (Abstract x y) = Abstract x (reduce y)
reduce x = x

main = putStrLn . show . reduce $
  Apply
    (Abstract "f" (Apply (Var "f") (Var "f")))
    (Abstract "f" (Apply (Var "f") (Var "f")))

