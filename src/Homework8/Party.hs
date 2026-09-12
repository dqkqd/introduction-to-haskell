module Homework8.Party where

import Data.Tree
import Homework8.Employee (Employee (empFun), GuestList (GL))

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e : es) (fun + empFun e)

moreFun :: GuestList -> GuestList -> GuestList
moreFun g1 g2
  | g1 > g2 = g1
  | otherwise = g2

treeFold :: (a -> b -> b) -> b -> Tree a -> b
treeFold f z (Node root forest) = f root (foldr (flip (treeFold f)) z forest)
