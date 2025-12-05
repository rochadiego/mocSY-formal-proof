module Main where

import Test.QuickCheck
import Verification.Properties (prop_incCheck, prop_equivalence)

main :: IO ()
main = do
    putStrLn "\n>>> Testing properties..."

    putStrLn "\n[1] Verifying system plus one"
    quickCheck prop_incCheck

    putStrLn "\n[2] Verifying system equivalence"
    quickCheck prop_equivalence
    
    putStrLn "\n>>> All tests have been completed"
