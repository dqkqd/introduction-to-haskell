module Homework8.Party where

import Homework8.Employee (Employee (empFun), GuestList (GL))

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e : es) (fun + empFun e)

moreFun :: GuestList -> GuestList -> GuestList
moreFun g1 g2
  | g1 > g2 = g1
  | otherwise = g2
