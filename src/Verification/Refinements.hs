{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Verification.Refinements where

import Models.PIDController(mySystem)
import ForSyDe.Shallow (Signal(..), mapSY)
import Prelude hiding (map)

-------------------------------------------------------------------------------
-- Signal Measurement
-------------------------------------------------------------------------------
{-@ measure lenSY @-}
{-@ lenSY :: Signal a -> Int @-}
lenSY :: Signal a -> Int
lenSY NullS   = 0
lenSY (x :- xs) = 1 + lenSY xs

-------------------------------------------------------------------------------
-- mapSY
-------------------------------------------------------------------------------
{-@ assume mapSY :: (a -> b) -> xs:Signal a -> {v:Signal b | lenSY v == lenSY xs} @-}

-------------------------------------------------------------------------------
-- mySystem
-------------------------------------------------------------------------------
{-@ assume mySystem :: xs:Signal Int -> {v:Signal Int | lenSY v == lenSY xs} @-}
