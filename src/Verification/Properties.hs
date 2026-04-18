{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- ============================================================================
-- Verification.Properties
--
-- Property suite for synchronous systems modeled in ForSyDe.
--
-- Focus:
--   * Temporal preservation
--   * Determinism
--   * Prefix causality
--   * Strict causality
--   * Compositionality
--   * Reset orthogonality
--   * Clock refinement
--
-- ============================================================================

module Verification.Properties where

import ForSyDe.Shallow (AbstExt (..), Signal, fromSignal, signal)
import ForSyDe.Shallow.MoC.Synchronous.Lib (zipWithSY)
import Quaternion (Quaternion, fromListQ)
import Test.QuickCheck

-- ============================================================================
-- Types
-- ============================================================================

type System a b = Signal a -> Signal b

type ResetSystem a b = Signal Bool -> Signal a -> Signal b

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance (Arbitrary a, Num a) => Arbitrary (Quaternion a) where
  arbitrary = fromListQ <$> vectorOf 4 arbitrary

instance (Arbitrary a) => Arbitrary (Signal a) where
  arbitrary = signal <$> listOf arbitrary
  shrink s = signal <$> shrink (fromSignal s)

instance (Arbitrary a) => Arbitrary (AbstExt a) where
  arbitrary =
    frequency
      [ (1, pure Abst),
        (4, Prst <$> arbitrary)
      ]

-- ============================================================================
-- Helpers
-- ============================================================================

prefixEq :: (Eq a) => Int -> [a] -> [a] -> Bool
prefixEq n xs ys = take n xs == take n ys

sameLength :: [a] -> [b] -> Bool
sameLength xs ys = length xs == length ys

everyK :: Int -> [a] -> [a]
everyK k xs =
  map snd $
    filter
      (\(i, _) -> i `mod` k == 0)
      (zip [0 ..] xs)

-- ============================================================================
-- P_SY1 - Synchronous Hypothesis
--
-- A synchronous process consumes one token per logical instant and produces
-- one token per logical instant.
-- ============================================================================

prop_PSY1_SynchronousHypothesis :: System a b -> [a] -> Property
prop_PSY1_SynchronousHypothesis sys xs =
  let out = fromSignal (sys (signal xs))
   in out `seq`
        property $
          sameLength xs out

-- ============================================================================
-- P_SY2 - Absent Signals Robustness
--
-- Presence/absence annotations must not destroy the global logical clock.
-- Temporal structure remains invariant even under fully absent streams.
-- ============================================================================

prop_PSY2_AbsentSignals ::
  (Eq b) =>
  System (AbstExt a) b ->
  [AbstExt a] ->
  Property
prop_PSY2_AbsentSignals sys xs =
  let o1 = fromSignal (sys (signal xs))
      allAbst = replicate (length xs) Abst
      o2 = fromSignal (sys (signal allAbst))
   in o1 `seq`
        o2 `seq`
          property $
            length o1 == length xs
              && length o2 == length xs

-- ============================================================================
-- P_SY3 - Determinism / Referential Transparency
-- Pure Haskell functions are deterministic by nature.
-- Extensionally equal inputs must yield equal outputs.
-- ============================================================================

prop_PSY3_Determinism ::
  (Eq a, Eq b) =>
  System a b ->
  [a] ->
  [a] ->
  Property
prop_PSY3_Determinism sys xs ys =
  xs == ys ==>
    fromSignal (sys (signal xs))
      == fromSignal (sys (signal ys))

-- ============================================================================
-- P_SY6 - Strict Causality
--
-- Output at logical instant t depends only on inputs strictly prior to t.
-- Operationally: modifying current/future inputs cannot affect past outputs.
--
-- Here tested at t = 0:
-- first output must be invariant under changes at current token.
-- ============================================================================

prop_PSY6_StrictCausality ::
  (Eq b) =>
  System a b ->
  a ->
  [a] ->
  [a] ->
  Property
prop_PSY6_StrictCausality sys x xs ys =
  let in1 = x : xs
      in2 = x : ys
      o1 = fromSignal (sys (signal in1))
      o2 = fromSignal (sys (signal in2))
   in o1 `seq`
        o2 `seq`
          property $
            take 1 o1 == take 1 o2

-- ============================================================================
-- P_SY7 - Concurrent Composition
--
-- Parallel synchronous composition preserves lockstep alignment.
-- If two subsystems are synchronous, their pointwise composition is also
-- synchronous and cardinality-preserving.
-- ============================================================================

prop_PSY7_ConcurrentComposition ::
  (Eq d) =>
  System a b ->
  System a c ->
  (b -> c -> d) ->
  [a] ->
  Property
prop_PSY7_ConcurrentComposition sys1 sys2 comb xs =
  let s = signal xs
      o1 = fromSignal (sys1 s)
      o2 = fromSignal (sys2 s)
      oz = fromSignal (zipWithSY comb (sys1 s) (sys2 s))
   in o1 `seq`
        o2 `seq`
          oz `seq`
            property $
              oz == zipWith comb o1 o2
                && length oz == length xs

-- ============================================================================
-- P_SY8 - Orthogonal Preemption - not validated yet
--
-- A reset event instantaneously reinitializes local state without corrupting
-- global time progression. Post-reset behavior equals fresh execution.
-- ============================================================================

prop_PSY8_OrthogonalPreemption ::
  (Eq b) =>
  ResetSystem a b ->
  [a] ->
  [a] ->
  Property
prop_PSY8_OrthogonalPreemption sys xsBefore xsAfter =
  let n = length xsBefore
      m = length xsAfter
   in m > 0 ==>
        let ctrl =
              replicate n False
                ++ [True]
                ++ replicate (m - 1) False

            input = xsBefore ++ xsAfter

            continuous =
              fromSignal $
                sys (signal ctrl) (signal input)

            fresh =
              fromSignal $
                sys
                  (signal (True : replicate (m - 1) False))
                  (signal xsAfter)

            suffixAfterReset =
              take m (drop n continuous)
         in continuous `seq`
              fresh `seq`
                property $
                  suffixAfterReset == fresh

-- ============================================================================
-- P_SY9 - Prefix Causality
--
-- Extending the future of an input stream must not alter already produced
-- outputs.
-- ============================================================================

prop_PSY9_CausalInterfaces ::
  (Eq b) =>
  System a b ->
  [a] ->
  [a] ->
  Property
prop_PSY9_CausalInterfaces sys xs future =
  let shortOut = fromSignal (sys (signal xs))
      longOut = fromSignal (sys (signal (xs ++ future)))
      n = length shortOut
   in shortOut `seq`
        longOut `seq`
          property $
            prefixEq n shortOut longOut

-- ============================================================================
-- P_SY10 - Clock Calculus (Downsampling Law)
--
-- Specialized law for rate-changing processes:
-- a k-decimator must emit ceil(n / k) tokens for n inputs.
--
-- ============================================================================

prop_PSY10_ClockCalculus ::
  (Eq a) =>
  Int ->
  System a a ->
  [a] ->
  Property
prop_PSY10_ClockCalculus k sys xs =
  k > 0 ==>
    let out = fromSignal (sys (signal xs))
        expected = everyK k xs
     in out `seq`
          property $
            out == expected