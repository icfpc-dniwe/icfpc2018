module ICFPC2018.Pack
  ( packMove
  , packSingleBotIntensions
  ) where

import Data.Maybe
import qualified Data.Map.Strict as M
import Linear.V3 (V3(..))
import Control.Monad.State.Strict
import Control.Arrow (first)

import ICFPC2018.Tensor3 (I3, Axis(..))
import qualified ICFPC2018.Tensor3 as T3
import ICFPC2018.Types
import ICFPC2018.Validation
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

linearDifferences :: Int -> [Difference]
linearDifferences maxLen = [ mkLinearDifference axis c
                           | axis <- [minBound..]
                           , c <- [-maxLen..maxLen]
                           , c /= 0
                           ]

longMoves :: [LongDifference]
longMoves = linearDifferences maxLLD

shortMoves :: [(ShortDifference, ShortDifference)]
shortMoves = [ (mkLinearDifference axis1 c1, mkLinearDifference axis2 c2)
             | axis1 <- [minBound..]
             , axis2 <- [minBound..]
             , c1 <- [-maxSLD..maxSLD]
             , c2 <- [-maxSLD..maxSLD]
             , c1 /= 0
             , c2 /= 0
             , axis1 /= axis2
             ]

{-# INLINE neighbours #-}
neighbours :: Model -> I3 -> I3 -> [(I3, Command)]
neighbours model finish p = finishStep ++ sSteps ++ lSteps
  where isPassable from step = all (not . (model T3.!)) (linearPath from step)

        finishStep
          | validLongDifference dFinish, Just move <- validSMove dFinish = [move]
          | Just (d1, d2) <- splitPlanar dFinish
          , validShortDifference d1 && validShortDifference d2 = mapMaybe validLMove [(d1, d2), (d2, d1)]
          | otherwise = []
          where dFinish = finish - p
        sSteps = mapMaybe validSMove longMoves
        lSteps = mapMaybe validLMove shortMoves

        validSMove d
          | T3.inBounds model newP &&
            isPassable p d
          = Just (newP, SMove d)
          | otherwise = Nothing
          where newP = p + d
        validLMove (d1, d2)
          | T3.inBounds model newP1 &&
            T3.inBounds model newP2 &&
            isPassable p d1 &&
            isPassable newP1 d2
          = Just (newP2, LMove d1 d2)
          | otherwise = Nothing
          where newP1 = p + d1
                newP2 = newP1 + d2

findPath :: Model -> I3 -> I3 -> Maybe [(I3, Command)]
findPath model start finish = aStar (neighbours model finish) mlenDistance start finish

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
       $ (map snd $ fromMaybe (error "unable to return to zero") $ findPath (filledModel m1) (singleBotPos m1) 0)
       ++ [Halt]

  packIntension :: Intension -> State SingleBotModel Trace
  packIntension = \case
    FillIdx idx -> do
      m@(SingleBotModel {..}) <- get

      let lowerVoxel = V3 0 (-1) 0
          upIdx      = idx + (V3 0 1 0)
          path       = map snd $ fromMaybe (error "unable move above a block") $ findPath filledModel singleBotPos upIdx

      put $ m { singleBotPos = upIdx, filledModel = T3.update filledModel [(idx, True)] }
      return $ singleBotCommandsToTrace botIdx $ path ++ [Fill lowerVoxel]

    FlipGravity -> return $ singleBotCommandsToTrace botIdx [Flip]
