{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import QuickSpec (quickSpec)
import Discovery.Signatures
  ( prop_incrementedRunningSum
  , prop_mapSY
  , prop_mySY
  )

main :: IO ()
main = do
    putStrLn "==============================================="
    putStrLn "   QuickSpec: Properties Discovery             "
    putStrLn "==============================================="

    quickSpec prop_mapSY
    quickSpec prop_mySY

    putStrLn "\n[3] Exploring the incremented-running-sum environment"
    quickSpec prop_incrementedRunningSum
    
    putStrLn "\n============================================="
    putStrLn "   End of exploration                          "
    putStrLn "==============================================="
