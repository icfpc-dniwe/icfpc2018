module ICFPC2018.Simulation where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Linear.V3 (V3(..))
import Linear.Vector ((*^))
import ICFPC2018.Types
import ICFPC2018.Utils
import qualified ICFPC2018.Tensor3 as T3

zero :: VolatileCoordinate
zero = V3 0 0 0

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
  | xFrom /= xTo = SMove (mkLinearDifference X distX) : packMove (V3 newX yFrom zFrom) (V3 xTo yTo zTo)
  | yFrom /= yTo = SMove (mkLinearDifference Y distY) : packMove (V3 xFrom newY zFrom) (V3 xTo yTo zTo)
  | zFrom /= zTo = SMove (mkLinearDifference Z distZ) : packMove (V3 xFrom yFrom newZ) (V3 xTo yTo zTo)
  | otherwise    = []
    where
      (distX, newX) = moveTowards xFrom xTo
      (distY, newY) = moveTowards yFrom yTo
      (distZ, newZ) = moveTowards zFrom zTo

moveTowards :: Int -> Int -> (Int, Int)
moveTowards from to | to > from + 15 = (15, from + 15)
                    | to < from - 15 = (-15, from - 15)
                    | otherwise      = (to - from, to)

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
                      { botPos :: !VolatileCoordinate
                      , filledModel :: !Model
                      } deriving (Show, Eq)

startModel :: V3 Int -> SingleBotModel
startModel sz = SingleBotModel {botPos = zero, filledModel = T3.replicate sz False}

packIntensions :: Intensions -> SingleBotModel -> Trace
packIntensions ((FillIdx idx):xs) (SingleBotModel {..}) = map (\c -> V.singleton c) (packMove botPos upIdx) ++ [V.singleton $ Fill lowerVoxel] ++ packIntensions xs (SingleBotModel {botPos = upIdx, ..})
  where
    lowerVoxel = V3 0 (-1) 0
    upIdx = case idx of
      (V3 x y z) -> (V3 x (y + 1) z)
packIntensions (FlipGravity:xs) model = V.singleton Flip : packIntensions xs model
packIntensions [] (SingleBotModel {..}) = map (\c -> V.singleton c) (packMove botPos zero) ++ [V.singleton Halt]
