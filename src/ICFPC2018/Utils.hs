{-# LANGUAGE LambdaCase #-}

module ICFPC2018.Utils where

import ICFPC2018.Types
import Linear.V3 (V3(..))
import Linear.Vector ((*^))

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
snakeIdx :: V3 Int -> [V3 Int]
snakeIdx (V3 xSize ySize zSize) = helpFold (V3 0 0 0) False
  where
    helpFold (V3 xIdx yIdx zIdx) ySwitch
      | yIdx >= ySize = []
      | firstX = map xToV3 [0 .. xSize - 1] ++ changeZ (xSize - 1)
      | lastX = map xToV3 [xSize - 1, xSize - 2 .. 0] ++ changeZ 0
      | otherwise = undefined
      where
        firstX = xIdx == 0
        -- firstY = yIdx == 0
        firstZ = zIdx == 0
        lastX = xIdx == xSize - 1
        -- lastY = yIdx == ySize - 1
        lastZ = zIdx == zSize - 1
        xToV3 x = (V3 x yIdx zIdx)
        changeZ x | lastZ && not ySwitch = changeY x
                  | firstZ && ySwitch    = changeY x
                  | not ySwitch = helpFold (V3 x yIdx (zIdx + 1)) ySwitch
                  | ySwitch = helpFold (V3 x yIdx (zIdx - 1)) ySwitch
                  | otherwise = error "snakeIdx: impossible"
        changeY x = helpFold (V3 x (yIdx + 1) zIdx) (not ySwitch)

mlen :: Integral a => V3 a -> a
mlen v = sum $ abs <$> v

clen :: Integral a => V3 a -> a
clen v = foldr1 max $ abs <$> v


mkLinearDifference :: Axis -> Int -> Difference
mkLinearDifference X v = V3 v 0 0
mkLinearDifference Y v = V3 0 v 0
mkLinearDifference Z v = V3 0 0 v

normalizeLinearDifference :: Difference -> Difference
normalizeLinearDifference (V3 x y z) = V3 (norm x) (norm y) (norm z) where
  norm v
    | v == 0    = 0
    | v >  0    = 1
    | otherwise = (-1)


packMove :: VolatileCoordinate -> VolatileCoordinate -> [Command]
packMove = error "TODO"


simulateStep
  :: VolatileCoordinate
  -- ^ We suppose that this coordinate is already in list of bot's trace
  -- ^ Probably there should be a system state
  -> Command
  -> [VolatileCoordinate]
simulateStep c = \case
  SMove d -> let
    nd = normalizeLinearDifference d
    ld = mlen d
    in map (\i -> c + i*^nd) [1..ld]

  LMove d1 d2 -> error "TODO: simulateStep LMove"
  cmd         -> error $ "TODO: simulateStep " ++ (show cmd)
