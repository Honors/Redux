module GrammarTests where

import Test
import Redux

grammarTests =
  describe "Grammar"
  >>= assert ((reduce (Apply (Abstract "x" (Var "x")) (Abstract "x" (Var "x")))) == (Abstract "x" (Var "x")))
  >>= assert ((reduce (Apply (Abstract "x" (Var "x")) (Abstract "x" (Var "x")))) == (Abstract "x" (Var "x")))

