module Homework5.Calc (
  eval,
  evalStr,
  Expr (lit, add, mul),
  reify,
  MinMax (MinMax),
  Mod7 (Mod7),
) where

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

class Expr a where
  lit :: Integer -> a
  add, mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
  lit v = v
  add = (+)
  mul = (*)

instance Expr Bool where
  lit v = v > 0
  add l r = l || r
  mul l r = l && r

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
  lit = MinMax
  add (MinMax l) (MinMax r) = MinMax $ max l r
  mul (MinMax l) (MinMax r) = MinMax $ min l r

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit v = Mod7 $ v `mod` 7
  add (Mod7 l) (Mod7 r) = Mod7 $ (l + r) `mod` 7
  mul (Mod7 l) (Mod7 r) = Mod7 $ (l * r) `mod` 7
