{-# LANGUAGE ScopedTypeVariables #-}

module Models.System
  ( genericSystem
  , systemModel
  , runningSum
  , incrementedRunningSum
  ) where

import ForSyDe.Shallow (Signal, AbstExt(..), mapSY, scanldSY, zipWithSY, delaySY)

-- | Running sum computed by a delay-driven accumulator.
runningSum :: Signal Int -> Signal Int
runningSum = scanldSY (+) 0

incrementedRunningSum :: Signal Int -> Signal Int
incrementedRunningSum = runningSum . mapSY (+1)

-- | A representative complex system handling AbstExt and Preemption via reset.
-- If reset is True, internal state should theoretically be bypassed (emulated here).
-- This acts as our generic "Model".
systemModel :: Signal Bool -> Signal (AbstExt Int) -> Signal (AbstExt Int)
systemModel reset input = zipWithSY process reset input
  where
    process r val = if r then Abst else val

-- | A generic combinator simulating a blackbox system that respects synchrony
genericSystem :: Signal Int -> Signal Int
genericSystem = mapSY (+1) . scanldSY (+) 0 . delaySY 0
