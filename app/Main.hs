{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import ForSyDe.Shallow (signal, fromSignal)
import Models.System (incrementedRunningSum)

main :: IO ()
main = do
    putStrLn "==========================================="
    putStrLn "   ForSyDe-Shallow: Simulation Example     "
    putStrLn "==========================================="

    let inputValues = [1, 2, 3, 4, 5, 5, 5, 4, 3, 2] :: [Int]
    let inputSignal = signal inputValues

    let outputSignal = incrementedRunningSum inputSignal

    putStrLn $ "Input: " ++ show inputValues
    putStrLn "-------------------------------------------"
    putStrLn $ "Output: " ++ show (fromSignal outputSignal)
    putStrLn "==========================================="
