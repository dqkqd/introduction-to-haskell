module Homework3 where

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
