import Test.QuickCheck
import Verification.Properties
import Models.System
import ForSyDe.Shallow (AbstExt(..), Signal, signal, fromSignal)
import ForSyDe.Shallow.MoC.Synchronous.Lib (zipWithSY)
import Quaternion (Quaternion)

main :: IO ()
main = do
    putStrLn "\n>>> Running Synchronous Verification for AAES Model..."
    
    putStrLn "\n[P_SY1] Synchronous Hypothesis (1:1 event mapping)"
    quickCheck (prop_PSY1_SynchronousHypothesis (aaesPN :: System (AbstExt (ImuVal, ImuVal, ImuVal)) [Vec]))

    putStrLn "\n[P_SY2] Absent Signals (Logical silence handling)"
    quickCheck (prop_PSY2_AbsentSignals (aaesPN :: System (AbstExt (ImuVal, ImuVal, ImuVal)) [Vec]))

    putStrLn "\n[P_SY3] Determinism (Functional purity)"
    quickCheck (prop_PSY3_Determinism (aaesPN :: System (AbstExt (ImuVal, ImuVal, ImuVal)) [Vec]))

    putStrLn "\n[P_SY6] Strict Causality (Productive feedback loops)"
    quickCheck (prop_PSY6_StrictCausality (aaesPN :: System (AbstExt (ImuVal, ImuVal, ImuVal)) [Vec]))

    putStrLn "\n[P_SY7] Concurrent Composition (Parallel consistency)"
    quickCheck (prop_PSY7_ConcurrentComposition 
        (aaesPN :: System (AbstExt (ImuVal, ImuVal, ImuVal)) [Vec])
        (aaesPN :: System (AbstExt (ImuVal, ImuVal, ImuVal)) [Vec])
        (++) )

    putStrLn "\n[P_SY8] Orthogonal Preemption (Single-cycle reaction)"
    let aaesPreempt sys rs xs = sys (zipWithSY (\r x -> if r then Abst else x) rs xs)
    quickCheck (prop_PSY8_OrthogonalPreemption (aaesPreempt aaesPN))

    putStrLn "\n[P_SY9] Causal Interfaces (Future independence)"
    quickCheck (prop_PSY9_CausalInterfaces (aaesPN :: System (AbstExt (ImuVal, ImuVal, ImuVal)) [Vec]))

    putStrLn "\n[P_SY10] Clock Calculus (Time alignment)"
    quickCheck (prop_PSY10_ClockCalculus (aaesPN :: System (AbstExt (ImuVal, ImuVal, ImuVal)) [Vec]))

    putStrLn "\n>>> All properties verified against the AAES model."
