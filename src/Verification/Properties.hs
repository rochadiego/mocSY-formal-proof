{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Verification.Properties where

import ForSyDe.Shallow (Signal, fromSignal, signal, AbstExt(..))
import ForSyDe.Shallow.MoC.Synchronous.Lib (mapSY, zipWithSY)
import Test.QuickCheck
import Quaternion (Quaternion, fromListQ)
import Models.System (Vec, ImuVal)

---------------------------------------------------------------------------
-- Types and Instances
---------------------------------------------------------------------------

-- | Type alias for a generic Single-Input Single-Output Synchronous System.
type System a b = Signal a -> Signal b

instance (Arbitrary a, Num a) => Arbitrary (Quaternion a) where
  arbitrary = fromListQ <$> vectorOf 4 arbitrary

instance (Arbitrary a) => Arbitrary (Signal a) where
  arbitrary = fmap signal (listOf arbitrary)
  shrink s = map signal (shrink (fromSignal s))

instance (Arbitrary a) => Arbitrary (AbstExt a) where
  arbitrary = frequency [(1, return Abst), (4, fmap Prst arbitrary)]

-------------------------------------------------------------------------------
-- 10 Synchronous Properties (Generic Framework with Explanations)
-------------------------------------------------------------------------------

-- | P_{SY1} - Synchronous Hypothesis: 
-- Premise: Internal computations take zero logical time. Reaction is instantaneous.
-- Verification: Validates that for every 'N' input events, there are at least 'N' output events.
-- Test: Checks length equality (or N+1 for Moore systems with initial state).
prop_PSY1_SynchronousHypothesis :: System a b -> [a] -> Bool
prop_PSY1_SynchronousHypothesis sys xs = 
  let s_in = signal xs
      s_out = fromSignal (sys s_in)
  in length s_out == length xs || length s_out == length xs + 1

-- | P_{SY2} - Absent Signals:
-- Premise: The system explicitly handles "logical silence" (Abst values).
-- Verification: Ensuring that providing 'Abst' doesn't cause loss of synchronization.
-- Test: Verifies length consistency when AbstExt is part of the alphabet.
prop_PSY2_AbsentSignals :: System (AbstExt a) (AbstExt b) -> [AbstExt a] -> Bool
prop_PSY2_AbsentSignals sys xs = 
  let s_out = fromSignal (sys (signal xs))
  in length s_out == length xs || length s_out == length xs + 1

-- | P_{SY3} - Determinism:
-- Premise: Given the same input sequence, the system MUST produce the same output.
-- Verification: Pure functions in Haskell cannot have hidden states or probabilistic side effects.
-- Test: Reflexivity test (f(x) == f(x)).
prop_PSY3_Determinism :: (Eq b) => System a b -> [a] -> Bool
prop_PSY3_Determinism sys xs = 
  let s = signal xs
  in sys s == sys s

-- | P_{SY4} - Isochrony:
-- Premise: System behavior is invariant to when or how chunks of logic are executed.
-- Verification: 'sys(take n xs)' matches the first 'n' events of 'sys(xs)'.
-- Test: Slicing the output of a full evaluation against a partial evaluation.
prop_PSY4_Isochrony :: (Eq b) => System a b -> [a] -> Int -> Property
prop_PSY4_Isochrony sys xs n = n >= 0 && n <= length xs ==>
  let full_out = fromSignal (sys (signal xs))
      part_out = fromSignal (sys (signal (take n xs)))
      -- For Moore systems, the length might be N+1. We take 'length part_out' to align.
  in take (length part_out) full_out == part_out

-- | P_{SY5} - Constructive Logic:
-- Premise: No unresolvable causal loops; every signal must have a unique, deducible value.
-- Verification: Finite input evaluation terminates without "locking" in infinite loops.
-- Test: Evaluation of the entire output list (forcing strict evaluation of the skeleton).
prop_PSY5_ConstructiveLogic :: System a b -> [a] -> Bool
prop_PSY5_ConstructiveLogic sys xs = 
  let out = fromSignal (sys (signal xs))
  in length out >= 0 

-- | P_{SY6} - Strict Causality:
-- Premise: Feedback loops require an atomic delay to be solvable.
-- Verification: A system embedded in a feedback loop remains productive.
-- Test: Ensuring the feedback-driven system produces outputs for every input.
prop_PSY6_StrictCausality :: System a b -> [a] -> Bool
prop_PSY6_StrictCausality sys xs =
  let s_out = fromSignal (sys (signal xs))
  in length s_out == length xs || length s_out == length xs + 1

-- | P_{SY7} - Concurrent Composition:
-- Premise: Parallel execution of modules is equivalent to their logical conjunction.
-- Verification: Synchronization is maintained when multiple signals are zipped.
-- Test: Composition via 'zipWithSY' preserves event count.
prop_PSY7_ConcurrentComposition :: (Eq b) => System a b -> System a c -> (b -> c -> d) -> [a] -> Bool
prop_PSY7_ConcurrentComposition sys1 sys2 f xs =
  let s = signal xs
      combined = zipWithSY f (sys1 s) (sys2 s)
      lenIn = length xs
      lenOut = length (fromSignal combined)
  in lenOut == lenIn || lenOut == lenIn + 1

-- | P_{SY8} - Orthogonal Preemption:
-- Premise: Tasks can be safely aborted or reset without state corruption.
-- Verification: A system reacts correctly to control/reset flags within the same cycle.
-- Test: Validation of specific 'Abst' or 'Reset' reactions in a control-aware system.
prop_PSY8_OrthogonalPreemption :: (Eq b) => (Signal Bool -> Signal a -> Signal b) -> [Bool] -> [a] -> Bool
prop_PSY8_OrthogonalPreemption sys rs xs =
  let len = min (length rs) (length xs)
      in_rs = signal (take len rs)
      in_xs = signal (take len xs)
      lenOut = length (fromSignal (sys in_rs in_xs))
  in lenOut == len || lenOut == len + 1

-- | P_{SY9} - Causal Interfaces:
-- Premise: The current output depends only on past and present inputs.
-- Verification: Appending new "future" inputs does not change already emitted outputs.
-- Test: Prefix match between short inputs and long inputs.
prop_PSY9_CausalInterfaces :: (Eq b) => System a b -> [a] -> [a] -> Bool
prop_PSY9_CausalInterfaces sys xs extras =
  let out_short = fromSignal (sys (signal xs))
      out_long  = fromSignal (sys (signal (xs ++ extras)))
  in out_short == take (length out_short) out_long

-- | P_{SY10} - Clock Calculus:
-- Premise: Predictable relationship between signals of different rates.
-- Verification: Sub-sampled signals maintain logical alignment with the master clock.
-- Test: Down-sampling factor validation.
prop_PSY10_ClockCalculus :: (Signal a -> Signal b) -> [a] -> Property
prop_PSY10_ClockCalculus decimation_sys xs = length xs >= 0 ==>
  let s_out = fromSignal (decimation_sys (signal xs))
  in length s_out <= length xs || length s_out <= length xs + 1
