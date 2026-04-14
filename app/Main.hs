{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import ForSyDe.Shallow (signal, fromSignal)
import Models.System (aaesPN, testInp)

main :: IO ()
main = do
    putStrLn "==========================================="
    putStrLn "   ForSyDe-Shallow: AAES PN Simulation     "
    putStrLn "==========================================="

    let outputSignal = aaesPN testInp

    putStrLn $ "Input: Simulation of 10 points with constant IMU data"
    putStrLn "-------------------------------------------"
    putStrLn $ "Output (euler, a_out, v_out, p_out, nzBody): " 
    putStrLn $ show (fromSignal outputSignal)
    putStrLn "==========================================="
