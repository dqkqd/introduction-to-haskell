module Homework5.Calc (eval) where

import Homework5.ExprT (ExprT (Add, Lit, Mul))

eval :: ExprT -> Integer
eval x = case x of
  Lit v -> v
  Add l r -> eval l + eval r
  Mul l r -> eval l * eval r
