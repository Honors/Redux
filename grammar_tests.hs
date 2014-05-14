module GrammarTests where

import Test
import Redux

grammarTests =
  describe "Grammar"
  >>= assertEq
	(reduce (Apply (Abstract "x" (Var "x")) (Abstract "x" (Var "x")))) 
	(Abstract "x" (Var "x"))
  >>= assertEq
	(reduce (Apply (Abstract "f" (Abstract "x" (Var "x"))) (Abstract "x" (Var "x"))))
	(Abstract "x" (Var "x"))

