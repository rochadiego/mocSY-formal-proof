import System.Exit (exitFailure)
import Test.QuickCheck
import Verification.Properties
import Models.System
import ForSyDe.Shallow (AbstExt(..))
import ForSyDe.Shallow.MoC.Synchronous.Lib (zipWithSY)

check :: Testable prop => prop -> IO ()
check p = do
    r <- quickCheckResult p
    case r of
      Success {} -> pure ()
      _ -> exitFailure

main :: IO ()
main = do
    putStrLn "\n>>> Running Synchronous Verification for AAES Model..."
    
    putStrLn "\n[P_SY1] Synchronous Hypothesis (1:1 event mapping)"
    check (prop_PSY1_SynchronousHypothesis (aaesPN :: System (AbstExt (ImuVal, ImuVal, ImuVal)) [Vec]))

    putStrLn "\n[P_SY2] Absent Signals (Logical silence handling)"
    check (prop_PSY2_AbsentSignals (aaesPN :: System (AbstExt (ImuVal, ImuVal, ImuVal)) [Vec]))

    putStrLn "\n[P_SY3] Determinism (Functional purity)"
    check (prop_PSY3_Determinism (aaesPN :: System (AbstExt (ImuVal, ImuVal, ImuVal)) [Vec]))

    putStrLn "\n[P_SY6] Strict Causality (Productive feedback loops)"
    check (prop_PSY6_StrictCausality (aaesPN :: System (AbstExt (ImuVal, ImuVal, ImuVal)) [Vec]))

    putStrLn "\n[P_SY7] Concurrent Composition (Parallel consistency)"
    check (prop_PSY7_ConcurrentComposition 
        (aaesPN :: System (AbstExt (ImuVal, ImuVal, ImuVal)) [Vec])
        (aaesPN :: System (AbstExt (ImuVal, ImuVal, ImuVal)) [Vec])
        (++) )

    putStrLn "\n[P_SY8] Orthogonal Preemption (Single-cycle reaction)"
    let aaesPreempt sys rs xs = sys (zipWithSY (\r x -> if r then Abst else x) rs xs)
    check (prop_PSY8_OrthogonalPreemption (aaesPreempt aaesPN))

    putStrLn "\n[P_SY9] Causal Interfaces (Future independence)"
    check (prop_PSY9_CausalInterfaces (aaesPN :: System (AbstExt (ImuVal, ImuVal, ImuVal)) [Vec]))

    putStrLn "\n[P_SY10] Clock Calculus (Time alignment)"
    check (prop_PSY10_ClockCalculus 1 (aaesPN :: System (AbstExt (ImuVal, ImuVal, ImuVal)) [Vec]))

    putStrLn "\n>>> All properties verified against the AAES model."
