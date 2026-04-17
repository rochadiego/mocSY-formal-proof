{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Verification.Properties where

import ForSyDe.Shallow (AbstExt (..), Signal, fromSignal, signal)
import ForSyDe.Shallow.MoC.Synchronous.Lib (zipWithSY)
import Quaternion (Quaternion, fromListQ)
import Test.QuickCheck

---------------------------------------------------------------------------
-- Types and Instances
---------------------------------------------------------------------------

-- | Generic synchronous system
type System a b = Signal a -> Signal b

instance (Arbitrary a, Num a) => Arbitrary (Quaternion a) where
  arbitrary = fromListQ <$> vectorOf 4 arbitrary

instance (Arbitrary a) => Arbitrary (Signal a) where
  arbitrary = fmap signal (listOf arbitrary)
  shrink s = map signal (shrink (fromSignal s))

instance (Arbitrary a) => Arbitrary (AbstExt a) where
  arbitrary = frequency [(1, return Abst), (4, fmap Prst arbitrary)]

---------------------------------------------------------------------------
-- Helper
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- Synchronous Properties (Corrected)
---------------------------------------------------------------------------


-- | P_SY1 - Synchronous Hypothesis
-- Every input tick produces exactly one output tick (no loss or gain of time)
prop_PSY1_SynchronousHypothesis :: System a b -> [a] -> Property
prop_PSY1_SynchronousHypothesis sys xs =
  let out = fromSignal (sys (signal xs))
   in out `seq` property (length out == length xs)


-- | P_SY2 - Absent Signals
-- Presence of Abst must not break temporal structure
prop_PSY2_AbsentSignals :: System (AbstExt a) b -> [AbstExt a] -> Property
prop_PSY2_AbsentSignals sys xs =
  let out1 = fromSignal (sys (signal xs))
      out2 = fromSignal (sys (signal (map (const Abst) xs)))
   in out1 `seq`
        out2 `seq`
          property
            ( length out1 == length xs
                && length out2 == length xs
            )


-- | P_SY3 - Determinism (Haskell Pure Function Native)
-- Same input must produce same output (evaluated twice)
prop_PSY3_Determinism :: (Eq b) => System a b -> [a] -> Bool
prop_PSY3_Determinism sys xs =
  let s = signal xs
      out1 = sys s
      out2 = sys s
   in out1 == out2


-- | P_SY6 - Strict Causality
-- Output at time t must NOT depend on future input (t+1, t+2, ...)
prop_PSY6_StrictCausality :: (Eq b) => System a b -> a -> a -> [a] -> Property
prop_PSY6_StrictCausality sys xNow xFuture xs =
  let inp1 = xNow : xNow : xs
      inp2 = xNow : xFuture : xs
      o1 = fromSignal (sys (signal inp1))
      o2 = fromSignal (sys (signal inp2))
   in o1 `seq`
        o2 `seq`
          property
            ( length o1 == length inp1
                && length o2 == length inp2
                && take 1 o1 == take 1 o2
            )


-- | P_SY7 - Concurrent Composition
-- Parallel systems must remain temporally aligned (lockstep)
prop_PSY7_ConcurrentComposition ::
  System a b -> System a c -> (b -> c -> d) -> [a] -> Property
prop_PSY7_ConcurrentComposition sys1 sys2 f xs =
  let s = signal xs
      sb1 = sys1 s
      sb2 = sys2 s
      o1 = fromSignal sb1
      o2 = fromSignal sb2
      out = fromSignal (zipWithSY f sb1 sb2)
   in o1 `seq`
        o2 `seq`
          out `seq`
            property
              ( length o1 == length xs
                  && length o2 == length xs
                  && length out == length xs
              )


-- | P_SY8 - Orthogonal Preemption
-- Reset must act immediately and not break time structure
prop_PSY8_OrthogonalPreemption ::
  (Eq b) =>
  (Signal Bool -> Signal a -> Signal b) ->
  [a] ->
  [a] ->
  Property
prop_PSY8_OrthogonalPreemption sys xs_before xs_after =
  let lenB = length xs_before
      lenA = length xs_after
   in lenA > 0 ==>
        let ctrl =
              replicate lenB False
                ++ [True]
                ++ replicate (lenA - 1) False
            input = xs_before ++ xs_after
            out_continuous =
              fromSignal (sys (signal ctrl) (signal input))
            out_fresh =
              fromSignal
                ( sys
                    (signal (True : replicate (lenA - 1) False))
                    (signal xs_after)
                )
            out_after_reset =
              drop lenB out_continuous
         in out_continuous `seq`
              out_fresh `seq`
                property (take lenA out_after_reset == out_fresh)


-- | P_SY9 - Causal Interfaces
-- Future inputs must not affect past outputs
prop_PSY9_CausalInterfaces :: (Eq b) => System a b -> [a] -> [a] -> Property
prop_PSY9_CausalInterfaces sys xs extras =
  let outShort = fromSignal (sys (signal xs))
      outLong = fromSignal (sys (signal (xs ++ extras)))
   in outShort `seq`
        property
          ( length outShort == length xs
              && take (length outShort) outLong == outShort
          )


-- | P_SY10 - Clock Calculus (Downsampling)
-- Output must follow a predictable subclock (factor k)
prop_PSY10_ClockCalculus ::
  Int -> (Signal a -> Signal b) -> [a] -> Property
prop_PSY10_ClockCalculus k sys xs =
  k > 0 ==>
    let out = fromSignal (sys (signal xs))
        expectedLen = (length xs + k - 1) `div` k
     in out `seq`
          property (length out == expectedLen)
