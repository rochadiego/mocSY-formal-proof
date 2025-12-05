{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Verification.Properties
  ( prop_incCheck,
    prop_equivalence,
  )
where

import ForSyDe.Shallow (Signal, fromSignal, mapSY, signal)
import Models.PIDController (mySystem)
import Test.QuickCheck

---------------------------------------------------------------------------
-- Arbitrary instance
---------------------------------------------------------------------------
instance (Arbitrary a) => Arbitrary (Signal a) where
  arbitrary = fmap signal (listOf arbitrary)
  shrink s = map signal (shrink (fromSignal s))

-------------------------------------------------------------------------------
-- Properties to be tested
-------------------------------------------------------------------------------
prop_incCheck :: [Int] -> Bool
prop_incCheck xs =
  let inputSig = signal xs
      outputSig = mySystem inputSig
      outputList = take (length xs) (fromSignal outputSig)
      expectedList = map (+ 1) xs
   in outputList == expectedList

prop_equivalence :: Signal Int -> Bool
prop_equivalence sig =
  let expected_system = mapSY (+ 1)
   in take 50 (fromSignal (mySystem sig))
        == take 50 (fromSignal (expected_system sig))
