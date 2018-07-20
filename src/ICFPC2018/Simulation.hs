module ICFPC2018.Simulation where

import Linear.V3 (V3(..))
import Linear.Vector ((*^))
import ICFPC2018.Types
import ICFPC2018.Utils

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
