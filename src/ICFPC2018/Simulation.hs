module ICFPC2018.Simulation where

import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Map as M
import Linear.V3 (V3(..))
import Linear.Vector ((*^))
import ICFPC2018.Types
import ICFPC2018.Utils
import qualified ICFPC2018.Tensor3 as T3
import Control.Monad.State.Strict


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
moveTowards from to | to > from + maxLLD = (maxLLD, from + maxLLD)
                    | to < from - maxLLD = (-maxLLD, from - maxLLD)
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


packIntensions :: SingleBotModel -> Intensions -> Trace
packIntensions m0 = concat . (flip evalState m0) . mapM packIntension where

  packIntension :: Intension -> State SingleBotModel Trace
  packIntension = \case
    FillIdx idx -> do
      SingleBotModel {..} <- get

      let lowerVoxel = V3 0 (-1) 0
      let upIdx      = idx + (V3 0 1 0)

      put (SingleBotModel {botPos = upIdx, ..})
      return (map V.singleton $ packMove botPos upIdx ++ [Fill lowerVoxel])

    FlipGravity -> return $ [V.singleton Flip]
