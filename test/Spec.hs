import System.Exit (exitFailure)
import Test.QuickCheck
import Verification.Properties
import Models.System
import ForSyDe.Shallow (AbstExt(..))

-- ============================================================================
-- QuickCheck Runner
-- ============================================================================

check :: Testable prop => prop -> IO ()
check p = do
  r <- quickCheckResult (withMaxSuccess 300 p)
  case r of
    Success {} -> pure ()
    _          -> exitFailure

-- ============================================================================
-- Main Verification Harness
-- Focused on properties semantically compatible with AAES model
-- ============================================================================

main :: IO ()
main = do
  putStrLn "\n>>> Running Synchronous Verification for AAES Model..."

  -- --------------------------------------------------------------------------
  -- P_SY1
  -- --------------------------------------------------------------------------
  putStrLn "\n[P_SY1] Synchronous Hypothesis (1:1 logical tick mapping)"
  check $
    prop_PSY1_SynchronousHypothesis
      (aaesPN :: System (AbstExt (ImuVal, ImuVal, ImuVal)) [Vec])

  -- --------------------------------------------------------------------------
  -- P_SY2
  -- --------------------------------------------------------------------------
  putStrLn "\n[P_SY2] Absent Signals Robustness (missing packet tolerance)"
  check $
    prop_PSY2_AbsentSignals
      (aaesPN :: System (AbstExt (ImuVal, ImuVal, ImuVal)) [Vec])

  -- --------------------------------------------------------------------------
  -- P_SY3
  -- --------------------------------------------------------------------------
  putStrLn "\n[P_SY3] Determinism (same input => same output)"
  check $ \xs ->
    prop_PSY3_Determinism
      (aaesPN :: System (AbstExt (ImuVal, ImuVal, ImuVal)) [Vec])
      xs
      xs

  -- --------------------------------------------------------------------------
  -- P_SY6
  -- --------------------------------------------------------------------------
  putStrLn "\n[P_SY6] Causality (future inputs do not affect present output)"
  check $
    prop_PSY6_StrictCausality
      (aaesPN :: System (AbstExt (ImuVal, ImuVal, ImuVal)) [Vec])

  -- --------------------------------------------------------------------------
  -- P_SY7
  -- --------------------------------------------------------------------------
  putStrLn "\n[P_SY7] Concurrent Composition (Parallel consistency)"
  check (prop_PSY7_ConcurrentComposition 
        (aaesPN :: System (AbstExt (ImuVal, ImuVal, ImuVal)) [Vec])
        (aaesPN :: System (AbstExt (ImuVal, ImuVal, ImuVal)) [Vec])
        (++) )

  -- --------------------------------------------------------------------------
  -- P_SY8 there is no reset in the model
  -- --------------------------------------------------------------------------
  -- putStrLn "\n[P_SY8] Orthogonal Preemption (Single-cycle reaction)"
  -- let aaesPreempt sys rs xs = sys (zipWithSY (\r x -> if r then Abst else x) rs xs)
  -- check (prop_PSY8_OrthogonalPreemption (aaesPreempt aaesPN))

  -- --------------------------------------------------------------------------
  -- P_SY9
  -- --------------------------------------------------------------------------
  putStrLn "\n[P_SY9] Prefix Causality (stream extension preserves past)"
  check $
    prop_PSY9_CausalInterfaces
      (aaesPN :: System (AbstExt (ImuVal, ImuVal, ImuVal)) [Vec])

  -- --------------------------------------------------------------------------
  -- P_SY10 not applicable to AAES model
  -- --------------------------------------------------------------------------
  -- putStrLn "\n[P_SY10] Clock Calculus (Time alignment)"
  -- check (prop_PSY10_ClockCalculus 1 (aaesPN :: System (AbstExt (ImuVal, ImuVal, ImuVal)) [Vec]))

  putStrLn "\n>>> All properties verified against the AAES model."
