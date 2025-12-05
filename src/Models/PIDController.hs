module Models.PIDController
  ( mySystem,
  )
where

import ForSyDe.Shallow (Signal, mapSY)

mySystem :: Signal Int -> Signal Int
mySystem = mapSY (+ 1)
