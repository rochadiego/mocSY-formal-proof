{-# LANGUAGE ScopedTypeVariables #-}

module Models.System
  ( runningSum
  , incrementedRunningSum
  ) where

import ForSyDe.Shallow (Signal)
import ForSyDe.Shallow.MoC.Synchronous.Lib (mapSY, scanldSY)

-- | Running sum computed by a delay-driven accumulator.
runningSum :: Signal Int -> Signal Int
runningSum = scanldSY (+) 0

-- | Increment each input before accumulating so the result can be
-- validated against list-level scan semantics.
incrementedRunningSum :: Signal Int -> Signal Int
incrementedRunningSum = runningSum . mapSY (+1)
