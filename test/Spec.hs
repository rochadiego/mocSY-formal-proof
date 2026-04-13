module Main where

import Test.QuickCheck
import Verification.Properties
import Models.System
import ForSyDe.Shallow (AbstExt(..), Signal, signal, fromSignal)
import ForSyDe.Shallow.MoC.Synchronous.Lib (zipWithSY)

main :: IO ()
main = do
    putStrLn "\n>>> Running Generic Synchronous Verification Framework..."
    
    putStrLn "\n[P_SY1] Synchronous Hypothesis (1:1 event mapping)"
    quickCheck (prop_PSY1_SynchronousHypothesis (genericSystem :: System Int Int))

    putStrLn "\n[P_SY2] Absent Signals (Logical silence handling)"
    let systemWithNoReset = systemModel (signal (repeat False))
    quickCheck (prop_PSY2_AbsentSignals (systemWithNoReset :: System (AbstExt Int) (AbstExt Int)))

    putStrLn "\n[P_SY3] Determinism (Functional purity)"
    quickCheck (prop_PSY3_Determinism (genericSystem :: System Int Int))

    putStrLn "\n[P_SY4] Isochrony (Timing invariance via lazy evaluation)"
    quickCheck (prop_PSY4_Isochrony (genericSystem :: System Int Int))

    putStrLn "\n[P_SY5] Constructive Logic (Liveness/Non-bottomness)"
    quickCheck (prop_PSY5_ConstructiveLogic (genericSystem :: System Int Int))

    putStrLn "\n[P_SY6] Strict Causality (Productive feedback loops)"
    quickCheck (prop_PSY6_StrictCausality (genericSystem :: System Int Int))

    putStrLn "\n[P_SY7] Concurrent Composition (Functional Parallelism)"
    quickCheck (prop_PSY7_ConcurrentComposition (genericSystem :: System Int Int) (genericSystem :: System Int Int) (+))

    putStrLn "\n[P_SY8] Orthogonal Preemption (Deterministic Reset/Abort)"
    quickCheck (prop_PSY8_OrthogonalPreemption (systemModel :: Signal Bool -> Signal (AbstExt Int) -> Signal (AbstExt Int)))

    putStrLn "\n[P_SY9] Causal Interfaces (Future independence)"
    quickCheck (prop_PSY9_CausalInterfaces (genericSystem :: System Int Int))

    putStrLn "\n[P_SY10] Clock Calculus (Synchronized rates)"
    -- Here val must be Int so that the result is AbstExt Int
    let downsampler (s :: Signal Int) = zipWithSY (\clk val -> if clk then Prst val else Abst) (signal (cycle [True, False])) s
    quickCheck (prop_PSY10_ClockCalculus (downsampler :: System Int (AbstExt Int)))

    putStrLn "\n>>> All properties verified against the model."
