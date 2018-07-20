module ICFPC2018.Utils where

import ICFPC2018.Types
import ICFPC2018.Tensor3 (Tensor3Size, Tensor3Idx)
import qualified ICFPC2018.Tensor3 as T3
import Linear.V3 (V3(..))

div1 :: Integral a => a -> a -> a
a `div1` b
  | m == 0 = d
  | otherwise = d + 1
  where (d, m) = a `divMod` b

infixl 7  `div1`

{-
moving across Tensor3 starting from (0, 0, 0) along X first then Z and finally Y.
Y == 0 is bottom plane.
-}
snakeIdx :: Tensor3Size -> [Tensor3Idx]
snakeIdx (V3 xSize ySize zSize) = helpFold (V3 0 0 0) False
  where
    helpFold (V3 xIdx yIdx zIdx) ySwitch
      | yIdx >= ySize = []
      | firstX = map xToV3 [0 .. xSize - 1] ++ changeZ (xSize - 1)
      | lastX = map xToV3 [xSize - 1, xSize - 2 .. 0] ++ changeZ 0
      | otherwise = undefined
      where
        firstX = xIdx == 0
        firstY = yIdx == 0
        firstZ = zIdx == 0
        lastX = xIdx == xSize - 1
        lastY = yIdx == ySize - 1
        lastZ = zIdx == zSize - 1
        xToV3 x = (V3 x yIdx zIdx)
        changeZ x | lastZ && not ySwitch = changeY x
                  | firstZ && ySwitch    = changeY x
                  | not ySwitch = helpFold (V3 x yIdx (zIdx + 1)) ySwitch
                  | ySwitch = helpFold (V3 x yIdx (zIdx - 1)) ySwitch
        changeY x = helpFold (V3 x (yIdx + 1) zIdx) (not ySwitch)

mkLinearDifference :: Axis -> Int -> V3 Int
mkLinearDifference X v = V3 v 0 0
mkLinearDifference Y v = V3 0 v 0
mkLinearDifference Z v = V3 0 0 v

mlen :: Integral a => V3 a -> a
mlen (V3 x y z) = sum $ abs <$> [x, y, z]

clen :: Integral a => V3 a -> a
clen (V3 x y z) = foldr1 (\x y -> max x y) $ abs <$> [x, y, z]

packMove :: VolatileCoordinate -> VolatileCoordinate -> [Command]
packMove = error "TODO"

simulateStep :: VolatileCoordinate -> Command -> [VolatileCoordinate]
simulateStep = error "TODO"
