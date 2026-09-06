module Homework3 (skips, localMaxima, histogram) where

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

{-
 -}
histogram :: [Integer] -> String
histogram a =
  let
    -- count the number of elements from 0..9 in a.
    -- E.g: [1,1,5] -> [0,2,0,0,0,1,0,0,0,0]
    count =
      zipWith
        ( \x y ->
            -- count the number of element equals y
            length $ filter (== y) x
        )
        (replicate 10 a)
        [0 .. 9]

    res =
      map
        ( ( \cs level ->
              -- only set '*' for those higher than level
              map
                (\c -> if c >= level then '*' else ' ')
                cs
          )
            count
        )
        -- list of level
        (reverse [1 .. maximum count])
   in
    unlines
      ( res
          ++
          -- add two bottom levels
          ["==========", "0123456789"]
      )
