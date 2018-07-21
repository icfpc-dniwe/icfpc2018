module ICFPC2018.Simulation where

import Data.Vector (Vector)
import qualified Data.Vector as V
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
-- stupid pack using only SMove and not checking collisions
packMove (V3 xFrom yFrom zFrom) (V3 xTo yTo zTo)
  | xFrom /= xTo = SMove (mkLinearDifference X (xTo - xFrom)) : packMove (V3 xTo yFrom zFrom) (V3 xTo yTo zTo)
  | yFrom /= yTo = SMove (mkLinearDifference Y (yTo - yFrom)) : packMove (V3 xFrom yTo zFrom) (V3 xTo yTo zTo)
  | zFrom /= zTo = SMove (mkLinearDifference Z (zTo - zFrom)) : packMove (V3 xFrom yFrom zTo) (V3 xTo yTo zTo)
  | otherwise    = []

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

data SingleBotModel = SingleBotModel
                      { botPos :: !(V3 Int)
                      , filledModel :: !Model
                      } deriving (Show, Eq)

packIntensions :: Intensions -> SingleBotModel -> Trace
packIntensions ((FillIdx idx):xs) (SingleBotModel {..}) = map (\c -> V.singleton c) (packMove botPos upIdx) ++ packIntensions xs (SingleBotModel {botPos = upIdx, ..})
  where
    upIdx = case idx of
      (V3 x y z) -> (V3 x (y + 1) z)
packIntensions (FlipGravity:xs) model = V.singleton Flip : packIntensions xs model
packIntensions [] _ = []
