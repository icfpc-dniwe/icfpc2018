module ICFPC2018.Validation
  ( validTrace
  , validLongDifference
  , validShortDifference
  , validNearDifference
  ) where

import Linear.V3 (V3(..))

import ICFPC2018.Types
import ICFPC2018.Utils

validLinearDifference :: Int -> V3 Int -> Bool
validLinearDifference maxDist (V3 x 0 0) = abs x <= maxDist
validLinearDifference maxDist (V3 0 y 0) = abs y <= maxDist
validLinearDifference maxDist (V3 0 0 z) = abs z <= maxDist
validLinearDifference _ _ = False

validLongDifference :: LongDifference -> Bool
validLongDifference = validLinearDifference maxLLD

validShortDifference :: ShortDifference -> Bool
validShortDifference = validLinearDifference maxSLD

validNearDifference :: NearDifference -> Bool
validNearDifference dist = mlen dist <= 2 && clen dist == 1

validCommand :: Command -> Bool
validCommand Halt = True
validCommand Wait = True
validCommand Flip = True
validCommand (SMove lld) = validLongDifference lld
validCommand (LMove sld1 sld2) = validShortDifference sld1 && validShortDifference sld2
validCommand (Fission nd m) = validNearDifference nd && m <= 0xFF
validCommand (Fill nd) = validNearDifference nd
validCommand (FusionP nd) = validNearDifference nd
validCommand (FusionS nd) = validNearDifference nd

validTrace :: Trace -> Bool
validTrace = all validStep
  where validStep = all validCommand
