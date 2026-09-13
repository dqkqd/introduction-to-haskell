{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Homework12.Risk where

import Control.Monad.Random
import Data.List (sortBy)
import Data.Ord (Down (Down), comparing)

------------------------------------------------------------
-- Die values

newtype DieValue = DV {unDV :: Int}
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random = first DV . randomR (1, 6)
  randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield {attackers :: Army, defenders :: Army}
  deriving (Show)

battle :: Battlefield -> Rand StdGen Battlefield
battle b =
  replicateM (min (attackers b - 1) 3) die >>= \attackerDies ->
    replicateM (min (defenders b) 2) die >>= \defenderDies ->
      let result =
            zipWith
              (>)
              (sortBy (comparing Down) attackerDies)
              (sortBy (comparing Down) defenderDies)
          wins = sum (map fromEnum result)
          loses = length result - wins
       in return
            Battlefield
              { attackers = attackers b - loses
              , defenders = defenders b - wins
              }

invade :: Battlefield -> Rand StdGen Battlefield
invade b =
  if win b || lose b
    then
      return b
    else
      battle b >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb b =
  traverse invade (replicate 1000 b) >>= \battles ->
    let
      wins = sum (map (fromEnum . win) battles)
      prob = fromIntegral wins / fromIntegral (length battles)
     in
      return prob

win :: Battlefield -> Bool
win b = defenders b <= 0

lose :: Battlefield -> Bool
lose b = attackers b < 2
