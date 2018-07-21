module ICFPC2018.Simulation where

import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Map as M
import Linear.V3 (V3(..))
import Linear.Vector ((*^))
import Control.Monad.State.Strict
import Control.Arrow (first)
import qualified Data.Map.Strict as MS

import ICFPC2018.Types
import ICFPC2018.Utils
import qualified ICFPC2018.Tensor3 as T3


data SingleBotModel = SingleBotModel {
    botPos      :: !VolatileCoordinate
  , filledModel :: !Model
  } deriving (Show, Eq)


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


simulateStep :: SingleBotModel -> Command -> Either String SingleBotModel
simulateStep = simulateStep' where

  simulateStep' m = \case
    SMove d -> let
      c  = botPos m
      c' = c + d
      in do
        checkBounds m c' >> checkVoidPath m c c'
        Right $ m {botPos = c'}

    LMove d1 d2 -> let
      c   = botPos m
      c'  = c  + d1
      c'' = c' + d2
      in do
        checkBounds m c'  >> checkVoidPath m c c'
        checkBounds m c'' >> checkVoidPath m c' c''
        Right $ m {botPos = c''}

    cmd -> error $ "TODO: simulateStep " ++ (show cmd)

  checkBounds :: SingleBotModel -> VolatileCoordinate -> Either String ()
  checkBounds m c = let
    (V3 mx my mz) = T3.size . filledModel $ m
    (V3 cx cy cz) = c
    in when (not $ 0 < cx && cx < mx && 0 < cy && cy < my && 0 < cz && cz < mz)
       $ Left $ "out of bounds: " ++ show c

  checkVoidPath :: SingleBotModel -> VolatileCoordinate -> VolatileCoordinate -> Either String ()
  checkVoidPath m c c' = let
    d = c' - c
    n = normalizeLinearDifference d
    l = mlen d
    in when (any (\i -> (filledModel m) T3.! (c + i*^n)) [0..l])
       $ Left $ "non void voxels between" ++ show c ++ " and " ++ show c'


startModel :: V3 Int -> SingleBotModel
startModel sz = SingleBotModel {botPos = zero, filledModel = T3.replicate sz False}

singleBotCommandsToTrace :: BotIdx -> [Command] -> Trace
singleBotCommandsToTrace bid cmds = M.fromList <$> zip [bid] <$> (\x -> [x]) <$> cmds where

packIntensions :: SingleBotModel -> Intensions -> Trace
packIntensions m xs = t1 ++ t2 ++ [] where
  (t1, m') = first concat . (flip runState m) . mapM packIntension $ xs
  t2 = singleBotCommandsToTrace 0 $ (packMove (botPos m') zero) ++ [Halt]

  packIntension :: Intension -> State SingleBotModel Trace
  packIntension = \case
    FillIdx idx -> do
      SingleBotModel {..} <- get

      let lowerVoxel = V3 0 (-1) 0
      let upIdx      = idx + (V3 0 1 0)

      put (SingleBotModel {botPos = upIdx, ..})
      return $ singleBotCommandsToTrace 0 $ packMove botPos upIdx ++ [Fill lowerVoxel]

    FlipGravity -> return $ singleBotCommandsToTrace 0 [Flip]

data MultiBotModel = MultiBotModel
                     { botNum :: !Int
                     , allBotPos :: ![VolatileCoordinate]
                     , filledModelMulti :: !Model
                     } deriving (Show, Eq)

startMultiModel :: V3 Int -> MultiBotModel
startMultiModel sz = MultiBotModel {botNum = 1, allBotPos = [V3 0 0 0], filledModelMulti = T3.replicate sz False}

packMultiBot :: MultiBotModel -> Intensions -> Trace
packMultiBot m0 intensions = undefined

type YPlane = Int

sliceIntensions :: Intensions -> MS.Map YPlane Intensions
sliceIntensions intensions = foldr updateHelper MS.empty intensions
  where
    updateHelper FlipGravity m = m
    updateHelper intension@(FillIdx (V3 _ y _)) m = MS.insertWith (++) y [intension] m
