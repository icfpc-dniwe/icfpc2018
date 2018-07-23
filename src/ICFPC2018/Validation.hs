module ICFPC2018.Validation
  ( validLongDifference
  , validShortDifference
  , validNearDifference
  , validFarDifference
  , fillablePoint
  ) where

import Linear.V3 (V3(..))

import ICFPC2018.Tensor3 (I3)
import qualified ICFPC2018.Tensor3 as T3
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

validFarDifference :: FarDifference -> Bool
validFarDifference dist = clen dist <= 30

fillablePoint :: Model -> I3 -> Bool
fillablePoint model = inBox (V3 1 0 1) (T3.size model - 2)

validFusion :: BotPos -> BotPos -> Bool
validFusion masterPos slavePos = validNearDifference (masterPos - slavePos)

possibleGFill :: [BotPos] -> Bool
possibleGFill = undefined

validGFill :: [(NearDifference, FarDifference)] -> Bool
validGFill = undefined
