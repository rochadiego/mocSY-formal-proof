module Main where

import Test.QuickCheck
import Verification.Properties

main :: IO ()
main = do
    putStrLn "\n>>> Testing 10 Synchronous Properties..."
    quickCheck prop_PSY1_SynchronousHypothesis
    quickCheck prop_PSY2_AbsentSignals
    quickCheck prop_PSY3_Determinism
    quickCheck prop_PSY4_Isochrony
    quickCheck prop_PSY5_ConstructiveLogic
    quickCheck prop_PSY6_StrictCausality
    quickCheck prop_PSY7_ConcurrentComposition
    quickCheck prop_PSY8_OrthogonalPreemption
    quickCheck prop_PSY9_CausalInterfaces
    quickCheck prop_PSY10_ClockCalculus
    putStrLn "\n>>> All tests have been completed"
