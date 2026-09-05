module Homework3 (skips, localMaxima) where

import Data.List (unsnoc)

{- |
>>> skips "ABCD"
["ABCD","BD","C","D"]
-}
skips :: [a] -> [[a]]
skips a =
  let
    n = length a
   in
    zipWith
      -- only take element in `a` that satisfy *skip step*.
      (\i v -> [x | (idx, x) <- v, idx `mod` i == 0])
      -- map each element of `[a,a,...]` with a *skip step*.
      [1 .. n]
      ( -- map each of the element in `a` with its index,
        -- then repeat `n` times to create `n a` list.
        replicate n $ zip [1 ..] a
      )

{- |
Create 2 shifted adjacent lists, then compare with the original one.
-}
localMaxima :: [Integer] -> [Integer]
localMaxima a =
  [ mid
  | (lhs, mid, rhs) <-
      zip3
        -- lhs: [5, 6, 1]
        (drop 2 a)
        -- mid: [9, 5, 6]
        (maybe [] fst (unsnoc $ drop 1 a))
        -- rhs: [2, 9, 5, 6, 1]
        a
  , mid > lhs && mid > rhs
  ]
