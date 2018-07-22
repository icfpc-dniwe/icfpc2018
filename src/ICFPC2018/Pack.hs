module ICFPC2018.Pack
  ( packMove
  , packSingleBotIntensions
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import Linear.V3 (V3(..))
import Linear.Vector ((*^))
import Control.Monad.State.Strict
import Control.Arrow (first)

import ICFPC2018.Tensor3 (I3)
import qualified ICFPC2018.Tensor3 as T3
import ICFPC2018.Types
import ICFPC2018.Utils
import ICFPC2018.Model

packMove :: I3 -> I3 -> [Command]
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
    = takeWhile (\i -> T3.inBounds m i && (not $ m T3.! i))
    . map (\j -> c + j *^ n)
    $ [1..l]

  genSL :: Model -> I3 -> Int -> [I3]
  genSL m c l = concatMap (genSLN m c l) dirs

  result m c
    =   map (\c' -> (c', SMove (c' - c))) (genSL m c 15)
    ++  concatMap (\c' -> map (\c'' -> (c'', LMove (c' - c) (c'' - c')))  (genSL m c' 5)) (genSL m c 5)

singleBotCommandsToTrace :: BotIdx -> [Command] -> Trace
singleBotCommandsToTrace bid cmds = M.fromList <$> zip [bid] <$> (\x -> [x]) <$> cmds where

data SingleBotModel = SingleBotModel { singleBotPos :: !VolatileCoordinate
                                     , filledModel :: !Model
                                     } deriving (Show, Eq)

packSingleBotIntensions :: Model -> BotIdx -> I3 -> Intensions -> Trace
packSingleBotIntensions model0 botIdx botPos0 xs = t1 ++ t2 where
  m0 = SingleBotModel { singleBotPos = botPos0, filledModel = model0 }
  (t1, m1) = first concat . (flip runState m0) . mapM packIntension $ xs

  t2 = singleBotCommandsToTrace botIdx
       $ (map snd $ fromMaybe (error "unable to return to zero") $ aStar (neighbours $ filledModel m1) mlenDistance (singleBotPos m1) 0)
       ++ [Halt]

  packIntension :: Intension -> State SingleBotModel Trace
  packIntension = \case
    FillIdx idx -> do
      m  <- get

      let lowerVoxel = V3 0 (-1) 0
      let upIdx      = idx + (V3 0 1 0)
      let m'         = m {singleBotPos = upIdx}

      put m'

      -- return $ singleBotCommandsToTrace botIdx $ packMove singleBotPos upIdx ++ [Fill lowerVoxel]
      return $ singleBotCommandsToTrace botIdx
        $ (map snd $ fromMaybe (error "unable to return to zero") $ aStar (neighbours $ filledModel m') mlenDistance (singleBotPos m') upIdx)
        ++ [Fill lowerVoxel]

    FlipGravity -> return $ singleBotCommandsToTrace 0 [Flip]
