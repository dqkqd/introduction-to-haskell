module Main where

import Control.Monad.Random (evalRandIO)
import Homework12.Risk (
  Battlefield (Battlefield, attackers, defenders),
  successProb,
 )

main :: IO ()
main =
  evalRandIO (successProb Battlefield{attackers = 50, defenders = 50}) >>= print
