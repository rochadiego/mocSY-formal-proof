{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import QuickSpec (quickSpec)
import Discovery.Signatures (prop_mapSY, prop_mySY)

main :: IO ()
main = do
    putStrLn "==============================================="
    putStrLn "   QuickSpec: Properties Discovery             "
    putStrLn "==============================================="

    quickSpec prop_mapSY
    quickSpec prop_mySY
    
    putStrLn "\n============================================="
    putStrLn "   End of exploration                          "
    putStrLn "==============================================="
