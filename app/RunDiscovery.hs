{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import QuickSpec (quickSpec)
import QuickSpec (quickSpec)
import Discovery.Signatures (prop_aaesPN)

main :: IO ()
main = do
    putStrLn "==============================================="
    putStrLn "   QuickSpec: AAES Properties Discovery        "
    putStrLn "==============================================="

    quickSpec prop_aaesPN
    
    putStrLn "\n============================================="
    putStrLn "   End of exploration                          "
    putStrLn "==============================================="
