module Homework7.Scrabble (Score (Score), scoreLine) where

import Data.Char (toUpper)
import Homework7.JoinList (JoinList (Single))

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty = Score 0

score :: Char -> Score
score = score' . toUpper

score' :: Char -> Score
score' 'A' = Score 1
score' 'B' = 3
score' 'C' = 3
score' 'D' = 2
score' 'E' = 1
score' 'F' = 4
score' 'G' = 2
score' 'H' = 4
score' 'I' = 1
score' 'J' = 8
score' 'K' = 5
score' 'L' = 1
score' 'M' = 3
score' 'N' = 1
score' 'O' = 1
score' 'P' = 3
score' 'Q' = 10
score' 'R' = 1
score' 'S' = 1
score' 'T' = 1
score' 'U' = 1
score' 'V' = 4
score' 'W' = 4
score' 'X' = 8
score' 'Y' = 4
score' 'Z' = 10
score' _ = 0

scoreLine :: String -> JoinList Score String
scoreLine s = Single (sum $ map score s) s
