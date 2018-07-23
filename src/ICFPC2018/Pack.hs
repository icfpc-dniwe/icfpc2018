module ICFPC2018.Pack
  ( packMove
  , packSingleBotIntensions
  , findPath
  ) where

import Data.Maybe
import qualified Data.IntMap.Strict as IM
import Linear.V3 (V3(..))
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.State.Strict

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

allSingleCommands :: [Command]
allSingleCommands = Wait : (map SMove longMoves)
                    ++ (map (uncurry LMove) shortMoves)
                    ++ (map Fill nearDiff)
                    ++ (map Void nearDiff)

fissionCommands :: Int -> [Command]
fissionCommands numSeeds = map (uncurry Fission) [(dist, m) | dist <- nearDiff, m <- [0..numSeeds]]

longMoves :: [LongDifference]
longMoves = linearDifferences maxLLD

nearDiff :: [NearDifference]
nearDiff = [(V3 x y z)
            | x <- [-1..1]
            , y <- [-1..1]
            , z <- [-1..1]
            , validNearDifference (V3 x y z)
            ]

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
singleBotCommandsToTrace bid cmds = IM.fromList <$> zip [bid] <$> (\x -> [x]) <$> cmds

packSingleBotIntensions :: Model -> BotIdx -> I3 -> Intensions -> Trace
packSingleBotIntensions model0 botIdx botPos0 xs = singleBotCommandsToTrace botIdx $ t1 ++ t2 where
  (t1, model1, pos1) = runST $ do
    modelM0 <- T3.thaw model0
    (t, pos) <- flip runStateT botPos0 $ fmap concat $ mapM (packIntension modelM0) xs
    model <- T3.freeze modelM0
    return (t, model, pos)

  t2 = (map snd $ fromMaybe (error "unable to return to zero") $ findPath model1 pos1 0) ++ [Halt]

  packIntension :: MModel s -> Intension -> StateT I3 (ST s) [Command]
  packIntension modelM intension =
    case intension of
      FillIdx idx -> do
        -- Be very cautious there!
        model <- lift $ T3.unsafeFreeze modelM
        botPos <- get
        let lowerVoxel = V3 0 (-1) 0
            upIdx      = idx + (V3 0 1 0)
            path       = map snd $ fromMaybe (error "unable move above a block") $ findPath model botPos upIdx
        -- We force the path to be computed before mutating the model
        lift $ T3.write modelM (path `seq` idx) True
        put upIdx
        return (path ++ [Fill lowerVoxel])
      FlipGravity -> return [Flip]
