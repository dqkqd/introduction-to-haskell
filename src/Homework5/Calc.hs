module Homework5.Calc (eval, evalStr) where

import Homework5.ExprT (ExprT (Add, Lit, Mul))
import Homework5.Parser (parseExp)

eval :: ExprT -> Integer
eval x = case x of
  Lit v -> v
  Add l r -> eval l + eval r
  Mul l r -> eval l * eval r

evalStr :: String -> Maybe Integer
evalStr program = do
  prog <- parseExp Lit Add Mul program
  return (eval prog)
