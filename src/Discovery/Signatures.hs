{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Discovery.Signatures (prop_aaesPN) where

import QuickSpec
import Test.QuickCheck
import Data.Proxy (Proxy(..))
import Models.System (aaesPN, ImuVal, Vec)
import ForSyDe.Shallow (Signal(..), fromSignal, signal, AbstExt(..))
import Quaternion (Quaternion, fromListQ, quat2list)

-------------------------------------------------------------------------------
-- Arbitrary Instances
-------------------------------------------------------------------------------
instance Arbitrary a => Arbitrary (Signal a) where
  arbitrary = fmap signal (listOf arbitrary)
  shrink s  = map signal (shrink (fromSignal s))

instance (Arbitrary a, Num a) => Arbitrary (Quaternion a) where
  arbitrary = fromListQ <$> vectorOf 4 arbitrary

instance (Arbitrary a) => Arbitrary (AbstExt a) where
  arbitrary = frequency [(1, return Abst), (4, fmap Prst arbitrary)]

instance (Ord a) => Ord (AbstExt a) where
  compare Abst Abst = EQ
  compare Abst (Prst _) = LT
  compare (Prst _) Abst = GT
  compare (Prst a) (Prst b) = compare a b

instance (Ord a) => Ord (Quaternion a) where
  compare q1 q2 = compare (quat2list q1) (quat2list q2)

-------------------------------------------------------------------------------
-- Observe instances
-------------------------------------------------------------------------------
instance Observe () [[Vec]] (Signal [Vec]) where
  observe _ = take 10 . fromSignal

-------------------------------------------------------------------------------
-- Signatures for AAES
-------------------------------------------------------------------------------
prop_aaesPN :: Sig
prop_aaesPN = signature
  [
    con "aaesPN" aaesPN,
    
    monoType (Proxy :: Proxy (AbstExt (ImuVal, ImuVal, ImuVal))),
    monoTypeObserve (Proxy :: Proxy (Signal [Vec]))
  ]
