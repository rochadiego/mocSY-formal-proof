{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Verification.Properties where

import ForSyDe.Shallow (Signal, fromSignal, signal, AbstExt(..))
import ForSyDe.Shallow.MoC.Synchronous.Lib (zipWithSY)
import Test.QuickCheck
import Quaternion (Quaternion, fromListQ)

---------------------------------------------------------------------------
-- Types and Instances
---------------------------------------------------------------------------

-- | Generic synchronous system
type System a b = Signal a -> Signal b

instance (Arbitrary a, Num a) => Arbitrary (Quaternion a) where
  arbitrary = fromListQ <$> vectorOf 4 arbitrary

instance (Arbitrary a) => Arbitrary (Signal a) where
  arbitrary = fmap signal (listOf arbitrary)
  shrink s  = map signal (shrink (fromSignal s))

instance (Arbitrary a) => Arbitrary (AbstExt a) where
  arbitrary = frequency [(1, return Abst), (4, fmap Prst arbitrary)]

---------------------------------------------------------------------------
-- Helper
---------------------------------------------------------------------------

sameLength :: [a] -> [b] -> Bool
sameLength xs ys = length xs == length ys

---------------------------------------------------------------------------
-- Synchronous Properties (Corrected)
---------------------------------------------------------------------------

-- | P_SY1 - Synchronous Hypothesis
-- Every input tick produces exactly one output tick (no loss or gain of time)
prop_PSY1_SynchronousHypothesis :: System a b -> [a] -> Bool
prop_PSY1_SynchronousHypothesis sys xs =
  let out = fromSignal (sys (signal xs))
  in sameLength xs out

-- | P_SY2 - Absent Signals
-- Presence of Abst must not break temporal structure
prop_PSY2_AbsentSignals :: System (AbstExt a) b -> [AbstExt a] -> Bool
prop_PSY2_AbsentSignals sys xs =
  let out = fromSignal (sys (signal xs))
  in sameLength xs out

-- | P_SY3 - Determinism (Haskell Pure Function Native)
-- Same input must produce same output (evaluated twice)
prop_PSY3_Determinism :: (Eq b) => System a b -> [a] -> Bool
prop_PSY3_Determinism sys xs =
  let s    = signal xs
      out1 = sys s
      out2 = sys s
  in out1 == out2

-- | P_SY6 - Strict Causality
-- Output at time t must NOT depend on input at time t
-- (change present input → output at same instant must remain unchanged)
prop_PSY6_StrictCausality :: (Eq b) => System a b -> a -> a -> [a] -> Property
prop_PSY6_StrictCausality sys x1 x2 xs =
  not (null xs) ==>
    let s1 = signal (x1 : xs)
        s2 = signal (x2 : xs)
        o1 = fromSignal (sys s1)
        o2 = fromSignal (sys s2)
    in head o1 == head o2

-- | P_SY7 - Concurrent Composition
-- Parallel systems must remain temporally aligned (lockstep)
prop_PSY7_ConcurrentComposition
  :: System a b -> System a c -> (b -> c -> d) -> [a] -> Bool
prop_PSY7_ConcurrentComposition sys1 sys2 f xs =
  let s   = signal xs
      o1  = fromSignal (sys1 s)
      o2  = fromSignal (sys2 s)
      out = fromSignal (zipWithSY f (sys1 s) (sys2 s))
  in sameLength o1 o2 && sameLength o1 out

-- | P_SY8 - Orthogonal Preemption
-- Reset must act immediately and not break time structure
prop_PSY8_OrthogonalPreemption :: (Eq b) => (Signal Bool -> Signal a -> Signal b) -> [a] -> [a] -> Bool
prop_PSY8_OrthogonalPreemption sys xs_before xs_after =
  let 
      ctrl = replicate (length xs_before) False ++ [True] ++ replicate (length xs_after - 1) False
      input = xs_before ++ xs_after
      out_continuous = fromSignal (sys (signal ctrl) (signal input))
      out_fresh = fromSignal (sys (signal (replicate (length xs_after) False)) (signal xs_after))
      out_after_reset = drop (length xs_before) out_continuous
  in 
      take (length out_fresh) out_after_reset == out_fresh

-- | P_SY9 - Causal Interfaces
-- Future inputs must not affect past outputs
prop_PSY9_CausalInterfaces :: (Eq b) => System a b -> [a] -> [a] -> Bool
prop_PSY9_CausalInterfaces sys xs extras =
  let outShort = fromSignal (sys (signal xs))
      outLong  = fromSignal (sys (signal (xs ++ extras)))
  in and (zipWith (==) outShort outLong)

-- | P_SY10 - Clock Calculus (Downsampling)
-- Output must follow a predictable subclock (factor k)
prop_PSY10_ClockCalculus
  :: (Eq b)
  => Int -> (Signal a -> Signal b) -> [a] -> Property
prop_PSY10_ClockCalculus k sys xs =
  k > 0 ==>
    let out = fromSignal (sys (signal xs))
        expectedLen = (length xs + k - 1) `div` k
    in length out == expectedLen

