{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Discovery.Signatures (prop_mapSY, prop_mySY) where

import QuickSpec
import Test.QuickCheck
import Models.PIDController (mySystem)
import ForSyDe.Shallow (Signal(..), fromSignal, signal, mapSY)

-------------------------------------------------------------------------------
-- Arbitrary Signal
-------------------------------------------------------------------------------
instance Arbitrary a => Arbitrary (Signal a) where
  arbitrary = fmap signal (listOf arbitrary)
  shrink s  = map signal (shrink (fromSignal s))

-------------------------------------------------------------------------------
-- Observe instance
-------------------------------------------------------------------------------
instance Observe () [Int] (Signal Int) where
  observe _ = take 100 . fromSignal

-------------------------------------------------------------------------------
-- Signatures to identify properties
-------------------------------------------------------------------------------
prop_mapSY :: Sig
prop_mapSY = signature
  [
    con "mySystem" mySystem,
    con "mapSY"    (mapSY @Int @Int),
    con "plus"     ((+) :: Int -> Int -> Int),
    con "const"    (const :: Int -> Int -> Int),
    con "."        ((.) :: (Signal Int -> Signal Int)
                         -> (Signal Int -> Signal Int)
                         -> Signal Int -> Signal Int),

    monoType    (Proxy :: Proxy Int),
    monoTypeObserve (Proxy :: Proxy (Signal Int))
  ]

prop_mySY :: Sig
prop_mySY = signature
  [
    con "mySystem" mySystem,
    con "mapSY"    (mapSY @Int @Int),
    con "plus"     ((+) :: Int -> Int -> Int),
    con "const"    (const :: Int -> Int -> Int),
    con "."        ((.) :: (Signal Int -> Signal Int)
                         -> (Signal Int -> Signal Int)
                         -> Signal Int -> Signal Int),

    monoType    (Proxy :: Proxy Int),
    monoTypeObserve (Proxy :: Proxy (Signal Int))
  ]
