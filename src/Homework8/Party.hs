module Homework8.Party where

import Data.Tree
import Homework8.Employee (Employee (empFun), GuestList (GL))

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e : es) (fun + empFun e)

moreFun :: GuestList -> GuestList -> GuestList
moreFun g1 g2
  | g1 > g2 = g1
  | otherwise = g2

-- trying to fold and calculate the sum of this tree
-- then at the root node, we must have sum of all
-- of its children.
-- Which means the function must be able to calculate
-- its value with all of its children
--    5
--  / | \
-- 1  2  3
-- But what do we have at the root node?
-- we don't have any children there
treeFold ::
  (a -> [b] -> b) -> -- a function that take a value at node `a`, a list of computed values in its children
  b -> -- a zeroed value (mempty)
  Tree a ->
  b
treeFold f z (Node root []) = f root [z]
treeFold f z (Node root children) = f root (map (treeFold f z) children)

-- a guess list with only one employee
only :: Employee -> GuestList
only e = glCons e mempty

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss members = (only boss, foldMap (uncurry moreFun) members)
