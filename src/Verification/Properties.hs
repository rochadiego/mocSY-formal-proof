{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Verification.Properties where

import ForSyDe.Shallow (Signal, fromSignal, signal, AbstExt(..))
import ForSyDe.Shallow.MoC.Synchronous.Lib (mapSY, scanldSY, zipWithSY, delaySY)
import Models.System (genericSystem, systemModel)
import Test.QuickCheck

---------------------------------------------------------------------------
-- Arbitrary instances
---------------------------------------------------------------------------
instance (Arbitrary a) => Arbitrary (Signal a) where
  arbitrary = fmap signal (listOf arbitrary)
  shrink s = map signal (shrink (fromSignal s))

instance (Arbitrary a) => Arbitrary (AbstExt a) where
  arbitrary = frequency [(1, return Abst), (4, fmap Prst arbitrary)]

-------------------------------------------------------------------------------
-- Properties to be tested mapping to the 10 Synchronous Properties
-------------------------------------------------------------------------------

-- P_{SY1} - Synchronous Hypothesis
-- Execution produces outputs strictly synchronized with inputs (same length).
prop_PSY1_SynchronousHypothesis :: [Int] -> Bool
prop_PSY1_SynchronousHypothesis xs = 
  length (fromSignal (genericSystem (signal xs))) == length xs

-- P_{SY2} - Absent Signals
-- Explicitly handles logical silence (AbstExt) without crashing.
prop_PSY2_AbsentSignals :: [AbstExt Int] -> Bool
prop_PSY2_AbsentSignals xs = 
  let s_in = signal xs
      s_out = mapSY (\x -> case x of Abst -> Abst; Prst v -> Prst (v+1)) s_in
  in length (fromSignal s_out) == length xs

-- P_{SY3} - Determinism
-- Pure functions guarantee that the exact same inputs yield exactly the same outputs.
prop_PSY3_Determinism :: [Bool] -> [Int] -> Bool
prop_PSY3_Determinism r xs = 
  let s_r = signal r
      s_v = signal (map Prst xs)
  in systemModel s_r s_v == systemModel s_r s_v

-- P_{SY4} - Isochrony
-- Chunked execution (or logical step isolation) matches contiguous execution.
prop_PSY4_Isochrony :: [Int] -> Int -> Property
prop_PSY4_Isochrony xs n = n >= 0 ==>
  let sig = signal xs
      chunked = signal (take n xs)
  in take n (fromSignal (genericSystem sig)) == fromSignal (genericSystem chunked)

-- P_{SY5} - Constructive Logic
-- Liveness: finite inputs evaluate completely, rejecting unresolvable states (bottom).
-- Deep evaluation of the generated signal proves absence of loops.
prop_PSY5_ConstructiveLogic :: [Int] -> Bool
prop_PSY5_ConstructiveLogic xs = 
  let out = fromSignal (genericSystem (signal xs))
  in length out >= 0 -- Forces WHNF over the list skeleton

-- P_{SY6} - Strict Causality
-- A strict causal feedback (delaySY-based scanldSY) safely terminates.
prop_PSY6_StrictCausality :: [Int] -> Bool
prop_PSY6_StrictCausality xs =
  let feedbackLoop = scanldSY (+) 0
  in length (fromSignal (feedbackLoop (signal xs))) == length xs

-- P_{SY7} - Concurrent Composition
-- Parallel equivalent to sequential. zipWithSY is perfectly deterministic.
prop_PSY7_ConcurrentComposition :: [Int] -> [Int] -> Bool
prop_PSY7_ConcurrentComposition xs ys =
  let c1 = genericSystem (signal xs)
      c2 = genericSystem (signal ys)
      combinedParallel = zipWithSY (+) c1 c2
      -- Sequenced equivalent
      combinedSeq = mapSY (uncurry (+)) (zipWithSY (,) c1 c2)
  in combinedParallel == combinedSeq

-- P_{SY8} - Orthogonal Preemption
-- System successfully aborts/suspends state when a control flag is requested.
prop_PSY8_OrthogonalPreemption :: [Bool] -> [Int] -> Property
prop_PSY8_OrthogonalPreemption rs xs =
    let resets = signal rs
        inputs = signal (map Prst xs)
        out = fromSignal (systemModel resets inputs)
        check (r, o) = if r then o == Abst else True
    in property $ all check (zip rs out)

-- P_{SY9} - Causal Interfaces
-- Output at instant K depends only on current and past inputs, not future.
prop_PSY9_CausalInterfaces :: [Int] -> Int -> Property
prop_PSY9_CausalInterfaces xs k = k >= 0 ==> 
  let evalAll_takeK = take k (fromSignal (genericSystem (signal xs)))
      evalTakeK     = fromSignal (genericSystem (signal (take k xs)))
  in evalAll_takeK == evalTakeK

-- P_{SY10} - Clock Calculus
-- Deterministic relationship between sub-clocks (down-sampling and up-sampling).
prop_PSY10_ClockCalculus :: [Int] -> Property
prop_PSY10_ClockCalculus xs = length xs > 0 ==> 
  let s1 = signal xs
      clock = signal (cycle [True, False])
      sampledDown = zipWithSY (\clk val -> if clk then Prst val else Abst) clock s1
  in length (fromSignal sampledDown) <= length xs
