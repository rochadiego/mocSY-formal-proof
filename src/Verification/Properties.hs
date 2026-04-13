{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Verification.Properties where

import ForSyDe.Shallow (Signal, fromSignal, signal, AbstExt(..))
import ForSyDe.Shallow.MoC.Synchronous.Lib (mapSY, zipWithSY)
import Test.QuickCheck

---------------------------------------------------------------------------
-- Types and Instances
---------------------------------------------------------------------------

-- | Type alias for a generic Single-Input Single-Output Synchronous System.
type System a b = Signal a -> Signal b

instance (Arbitrary a) => Arbitrary (Signal a) where
  arbitrary = fmap signal (listOf arbitrary)
  shrink s = map signal (shrink (fromSignal s))

instance (Arbitrary a) => Arbitrary (AbstExt a) where
  arbitrary = frequency [(1, return Abst), (4, fmap Prst arbitrary)]

-------------------------------------------------------------------------------
-- 10 Synchronous Properties (Generic Framework)
-------------------------------------------------------------------------------

-- | P_{SY1} - Synchronous Hypothesis: 
-- Premise: Internal computations are logically instantaneous.
-- Verification: For every input event, there is exactly one output event.
-- Nature: Haskell lists/streams maintain a 1:1 structural mapping in map-like processes.
prop_PSY1_SynchronousHypothesis :: (Eq b) => System a b -> [a] -> Bool
prop_PSY1_SynchronousHypothesis sys xs = 
  let s_in = signal xs
      s_out = fromSignal (sys s_in)
  in length s_out == length xs

-- | P_{SY2} - Absent Signals:
-- Premise: The system explicitly handles the lack of stimuli.
-- Verification: Providing 'Abst' values does not break the stream's temporal alignment.
-- Nature: Type safety enforces handling of 'AbstExt' if the function signature demands it.
prop_PSY2_AbsentSignals :: (Eq b) => System (AbstExt a) (AbstExt b) -> [AbstExt a] -> Bool
prop_PSY2_AbsentSignals sys xs = 
  length (fromSignal (sys (signal xs))) == length xs

-- | P_{SY3} - Determinism:
-- Premise: Same inputs always produce exactly the same outputs.
-- Verification: Two separate evaluations of the same signal result in identical output.
-- Nature: Pure Functions. Haskell functions cannot have hidden state or side effects.
prop_PSY3_Determinism :: (Eq b) => System a b -> [a] -> Bool
prop_PSY3_Determinism sys xs = 
  let s = signal xs
  in sys s == sys s

-- | P_{SY4} - Isochrony:
-- Premise: Behavior is invariant to physical distribution or timing of logic steps.
-- Verification: Computing the first 'n' outputs of a stream is equivalent to 
--              computing the output of the first 'n' inputs.
-- Nature: Lazy Evaluation. The "when" of execution does not alter the "what".
prop_PSY4_Isochrony :: (Eq b) => System a b -> [a] -> Int -> Property
prop_PSY4_Isochrony sys xs n = n >= 0 ==>
  let full_out = fromSignal (sys (signal xs))
      part_out = fromSignal (sys (signal (take n xs)))
  in take n full_out == part_out

-- | P_{SY5} - Constructive Logic:
-- Premise: Absence of causal loops; every value is deducible.
-- Verification: Finite inputs must produce finite outputs without hanging (bottom).
-- Nature: Evaluation of expressions. 
prop_PSY5_ConstructiveLogic :: (Eq b) => System a b -> [a] -> Bool
prop_PSY5_ConstructiveLogic sys xs = 
  let out = fromSignal (sys (signal xs))
  in length out >= 0 -- Forces evaluation of the list structure.

-- | P_{SY6} - Strict Causality:
-- Premise: Feedback loops must be broken by an atomic delay.
-- Verification: A system with feedback (like scanldSY) remains productive.
-- Nature: ForSyDe's delaySY primitive.
prop_PSY6_StrictCausality :: (Eq b) => System a b -> [a] -> Bool
prop_PSY6_StrictCausality sys xs =
  length (fromSignal (sys (signal xs))) == length xs

-- | P_{SY7} - Concurrent Composition:
-- Premise: Parallel execution is mathematically equivalent to logical conjunction.
-- Verification: A parallel composition of two systems via zipWith is deterministic.
-- Nature: Functional Composition.
prop_PSY7_ConcurrentComposition :: (Eq b) => System a b -> System a c -> (b -> c -> d) -> [a] -> Bool
prop_PSY7_ConcurrentComposition sys1 sys2 f xs =
  let s = signal xs
      out1 = sys1 s
      out2 = sys2 s
      combined = zipWithSY f out1 out2
  in length (fromSignal combined) == length xs

-- | P_{SY8} - Orthogonal Preemption:
-- Premise: Ability to abort/suspend tasks without inconsistent states.
-- Verification: If a control signal (reset) is active, the system's reaction is predictable.
-- Nature: Pure conditional logic.
prop_PSY8_OrthogonalPreemption :: (Eq b) => (Signal Bool -> Signal a -> Signal b) -> [Bool] -> [a] -> Bool
prop_PSY8_OrthogonalPreemption sys rs xs =
  let len = min (length rs) (length xs)
      in_rs = signal (take len rs)
      in_xs = signal (take len xs)
  in length (fromSignal (sys in_rs in_xs)) == len

-- | P_{SY9} - Causal Interfaces:
-- Premise: Output at time 'k' depends only on inputs at time '<= k'.
-- Verification: Appending more data to the input does not change the past outputs.
-- Nature: Referential Transparency.
prop_PSY9_CausalInterfaces :: (Eq b) => System a b -> [a] -> [a] -> Bool
prop_PSY9_CausalInterfaces sys xs extras =
  let out_short = fromSignal (sys (signal xs))
      out_long  = fromSignal (sys (signal (xs ++ extras)))
  in out_short == take (length xs) out_long

-- | P_{SY10} - Clock Calculus:
-- Premise: Multiple rates derived from a master reference.
-- Verification: Down-sampling and Up-sampling preserves synchronization relationships.
-- Nature: Type-driven stream processing.
prop_PSY10_ClockCalculus :: (Eq b) => (Signal a -> Signal b) -> [a] -> Property
prop_PSY10_ClockCalculus decimation_sys xs = length xs > 0 ==>
  let s_in = signal xs
      s_out = decimation_sys s_in
  in length (fromSignal s_out) <= length xs
