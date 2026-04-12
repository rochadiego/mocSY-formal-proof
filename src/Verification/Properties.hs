{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Verification.Properties
  ( prop_incrementedRunningSum
  )
where

import ForSyDe.Shallow (Signal, fromSignal, signal)
import Models.System (incrementedRunningSum)
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
prop_incrementedRunningSum :: [Int] -> Bool
prop_incrementedRunningSum xs =
  fromSignal (incrementedRunningSum (signal xs)) == scanl (+) 0 (map (+1) xs)
