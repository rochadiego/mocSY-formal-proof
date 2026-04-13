{-# LANGUAGE ScopedTypeVariables #-}

module Models.System
  ( combinatorialSystem
  , genericSystem
  , systemModel
  , runningSum
  , incrementedRunningSum
  ) where

import ForSyDe.Shallow (Signal, AbstExt(..), mapSY, scanldSY, zipWithSY, delaySY)

-- | A purely combinatorial system (Mealy architecture).
-- It has zero internal state, so output length always matches input length.
combinatorialSystem :: Signal Int -> Signal Int
combinatorialSystem = mapSY (+1)

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

-- | A strictly synchronous combinator (length-preserving).
-- It represents a system where output length matches input length exactly.
genericSystem :: Signal Int -> Signal Int
genericSystem = mapSY (+1) . scanldSY (+) 0
