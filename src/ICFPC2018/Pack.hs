module ICFPC2018.Pack where

import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Linear.V3 (V3(..))
import Linear.Vector ((*^))
import ICFPC2018.Types
import ICFPC2018.Utils
import ICFPC2018.Model
import ICFPC2018.Tensor3 (I3)
import Control.Monad.State.Strict
import Control.Arrow (first)

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


mlenMetric :: I3 -> I3 -> Int
mlenMetric x x' = mlen (x - x')

neighbours :: Model -> I3 -> [(I3, Command)]
neighbours = result where

  dirs :: [I3]
  dirs = [
      V3 1    0    0
    , V3 0    1    0
    , V3 0    0    1
    , V3 (-1) 0    0
    , V3 0    (-1) 0
    , V3 0    0    (-1)
    ]

  genSLN :: Model -> I3 -> Int -> I3 -> [I3]
  genSLN m c l n
    = takeWhile (\i -> checkBounds (T3.size m) i && (not $ m T3.! i))
    . map (\j -> c + j *^ n)
    $ [1..l]

  genSL :: Model -> I3 -> Int -> [I3]
  genSL m c l = concatMap (genSLN m c l) dirs

  result m c
    =   map (\c' -> (c', SMove (c' - c))) (genSL m c 15)
    ++  concatMap (\c' -> map (\c'' -> (c'', LMove (c' - c) (c'' - c')))  (genSL m c' 5)) (genSL m c 5)

simulateStep :: SingleBotModel -> Command -> Either String SingleBotModel
simulateStep = simulateStep' where

  simulateStep' m = \case
    SMove d -> let
      c  = botPos m
      c' = c + d
      in do
        checkStepBounds m c' >> checkVoidPath m c c'
        Right $ m {botPos = c'}

    LMove d1 d2 -> let
      c   = botPos m
      c'  = c  + d1
      c'' = c' + d2
      in do
        checkStepBounds m c'  >> checkVoidPath m c c'
        checkStepBounds m c'' >> checkVoidPath m c' c''
        Right $ m {botPos = c''}

    cmd -> error $ "TODO: simulateStep " ++ (show cmd)

  checkStepBounds :: SingleBotModel -> VolatileCoordinate -> Either String ()
  checkStepBounds m c = let
    (V3 mx my mz) = T3.size . filledModel $ m
    (V3 cx cy cz) = c
    in when (not $ 0 <= cx && cx < mx && 0 <= cy && cy < my && 0 <= cz && cz < mz)
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
packIntensions m0 xs = t1 ++ t2 where
  (t1, m1) = first concat . (flip runState m0) . mapM packIntension $ xs

  -- t2 = singleBotCommandsToTrace 0 $ (packMove (botPos m1) zero) ++ [Halt]
  t2 = singleBotCommandsToTrace 0
       $ (map snd $ fromMaybe (error "unable to return to zero") $ aStar (neighbours $ filledModel m1) mlenMetric (botPos m1) zero)
       ++ [Halt]

  packIntension :: Intension -> State SingleBotModel Trace
  packIntension = \case
    FillIdx idx -> do
      m  <- get

      let lowerVoxel = V3 0 (-1) 0
      let upIdx      = idx + (V3 0 1 0)
      let m'         = m {botPos = upIdx}

      put m'

      -- return $ singleBotCommandsToTrace 0 $ packMove botPos upIdx ++ [Fill lowerVoxel]
      return $ singleBotCommandsToTrace 0
        $ (map snd $ fromMaybe (error "unable to return to zero") $ aStar (neighbours $ filledModel m') mlenMetric (botPos m') upIdx)
        ++ [Fill lowerVoxel]

    FlipGravity -> return $ singleBotCommandsToTrace 0 [Flip]

type YPlane = Int

sliceIntensions :: Intensions -> Map YPlane Intensions
sliceIntensions intensions = foldr updateHelper M.empty intensions
  where
    updateHelper FlipGravity m = m
    updateHelper intension@(FillIdx (V3 _ y _)) m = M.insertWith (++) y [intension] m