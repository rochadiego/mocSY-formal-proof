module Main where

import Test.QuickCheck
import Verification.Properties (prop_incrementedRunningSum)

main :: IO ()
main = do
    putStrLn "\n>>> Testing properties..."

    putStrLn "\n[1] Validating the incremented running-sum model"
    quickCheck prop_incrementedRunningSum
    
    putStrLn "\n>>> All tests have been completed"
