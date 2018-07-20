{-# LANGUAGE LambdaCase #-}

module ICFPC2018.Simulation where

import Linear.Vector ((*^))
import ICFPC2018.Types
import ICFPC2018.Utils


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
