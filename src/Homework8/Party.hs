module Homework8.Party where

import Homework8.Employee (Employee (empFun), GuestList (GL))

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e : es) (fun + empFun e)
