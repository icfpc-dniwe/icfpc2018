module ICFPC2018.Simulation
  ( ExecState(..)
  , BotState(..)
  , initialState
  , stepState
  ) where

import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import qualified Data.Vector as V
import Linear.V3 (V3(..))

import ICFPC2018.Types
import ICFPC2018.Utils
import ICFPC2018.Tensor3 (I3)
import qualified ICFPC2018.Tensor3 as T3
import ICFPC2018.Validation

--import Debug.Trace

data ExecState = ExecState { stateEnergy :: !Int
                           , stateHarmonics :: !HarmonicState
                           , stateMatrix :: !Model
                           , stateBots :: !(Map BotIdx BotState)
                           , stateHalted :: !Bool
                           }
                           deriving (Show, Eq)

type SeedsSet = IntSet

type BotPositions = Map I3 BotIdx

data FusionStatus = FusionMaster | FusionSlave
                  deriving (Show, Eq)

data BotState = BotState { botPos :: !I3
                         , botSeeds :: !SeedsSet
                         }
              deriving (Show, Eq)

data SilulationVoxelAction = VoxelFill | VoxelVoid
                           deriving (Show, Eq)
actionToBool :: SilulationVoxelAction -> Bool
actionToBool VoxelFill = True
actionToBool VoxelVoid = False

initialState :: Int -> ExecState
initialState r = ExecState { stateEnergy = 0
                           , stateHarmonics = Low
                           , stateMatrix = T3.create (V.replicate (product size) False) size
                           , stateBots = M.singleton 1 initialBot
                           --, stateGFillProcessedBots = IS.empty
                           --, stateGVoidProcessedBots = IS.empty
                           , stateHalted = False
                           }
  where initialBot = BotState { botPos = 0
                              , botSeeds = IS.fromList [2..20]
                              }
        size = V3 r r r

stepState :: ExecState -> Step -> Maybe ExecState
stepState state@(ExecState {..}) step = do
  guard $ not stateHalted
  guard $ M.keys step == M.keys stateBots
  let harmonicsCost = (if stateHarmonics == Low then 3 else 30) * product (T3.size stateMatrix)
      botsCost = 20 * M.size stateBots
      state1 = state { stateEnergy = stateEnergy + harmonicsCost + botsCost }
  let botPositions = M.fromList $ map (\(idx, bot) -> (botPos bot, idx)) $ M.toList stateBots
  (state2, _) <- foldM (stepBot botPositions step) (state1, M.keysSet botPositions) $ M.toList step
  -- FIXME: check connectivity
  return state2

botsPerformingCommand :: Step -> (Command -> Bool) -> [BotIdx]
botsPerformingCommand step pr = M.keys $ M.filter pr step

botsPerformingGFill :: Step -> [BotIdx]
botsPerformingGFill step = botsPerformingCommand step (
  \cmd -> case cmd of
    (GFill _ _) -> True
    _           -> False
  )

botsPerformingGVoid :: Step -> [BotIdx]
botsPerformingGVoid step = botsPerformingCommand step (
  \cmd -> case cmd of
    (GVoid _ _) -> True
    _           -> False
  )

stepBot :: BotPositions -> Step -> (ExecState, Set VolatileCoordinate) -> (BotIdx, Command) -> Maybe (ExecState, Set VolatileCoordinate)
stepBot botPositions step (state@ExecState {..}, volatiles) (botIdx, command) =
  case command of
    Halt -> do
      guard $ M.size stateBots == 1 && myPos == 0
      return (state { stateHalted = True }, volatiles)
    Wait -> Just (state, volatiles)
    Flip -> Just (state { stateHarmonics = changeHarmonic stateHarmonics }, volatiles)
    SMove lld -> do
      let newPos = myPos + lld
      guard $ validLongDifference lld && T3.inBounds stateMatrix newPos
      volatiles' <- addVolatiles state volatiles (S.fromList $ linearPath myPos lld)
      let newBots = M.insert botIdx (botState { botPos = newPos }) stateBots
      return (state { stateBots = newBots, stateEnergy = stateEnergy + 2 * mlen lld }, volatiles')
    LMove sld1 sld2 -> do
      let cornerPos = myPos + sld1
          newPos = cornerPos + sld2
      guard $ validShortDifference sld1 && validShortDifference sld2 && T3.inBounds stateMatrix cornerPos && T3.inBounds stateMatrix newPos
      volatiles' <- addVolatiles state volatiles (S.fromList $ linearPath myPos sld1 ++ linearPath cornerPos sld2)
      let newBots = M.insert botIdx (botState { botPos = newPos }) stateBots
      return (state { stateBots = newBots, stateEnergy = stateEnergy + 2 * (clen sld1 + clen sld2 + 2) }, volatiles')
    Fill nd -> do
      let pos = myPos + nd
      guard $ validNearDifference nd && T3.inBounds stateMatrix pos
      updateRegion VoxelFill (state, volatiles) [pos]
    Void nd -> do
      let pos = myPos + nd
      guard $ validNearDifference nd && T3.inBounds stateMatrix pos
      updateRegion VoxelVoid (state, volatiles) [pos]
    Fission nd m -> do
      let (childId:childSeeds, parentSeeds) = splitAt (m + 1) $ IS.toAscList $ botSeeds botState
          childPos = myPos + nd
      guard $ validNearDifference nd && T3.inBounds stateMatrix childPos
      volatiles' <- addVolatiles state volatiles (S.singleton childPos)
      let newBots = M.insert botIdx (botState { botSeeds = IS.fromList parentSeeds }) $
                    M.insert childId (BotState { botPos = childPos
                                               , botSeeds = IS.fromList childSeeds
                                               }) $
                    stateBots
      return (state { stateBots = newBots, stateEnergy = stateEnergy + 24 }, volatiles')
    FusionP nd -> do
      let childPos = myPos + nd
      guard $ validNearDifference nd && T3.inBounds stateMatrix childPos
      childIdx <- M.lookup childPos botPositions
      let FusionS childNd = step M.! childIdx
      guard $ childPos + childNd == myPos
      let childState = stateBots M.! botIdx
      let newBots = M.insert botIdx (botState { botSeeds = IS.insert childIdx $ botSeeds botState `IS.union` botSeeds childState }) $
                    M.delete childIdx stateBots
      return (state { stateBots = newBots, stateEnergy = stateEnergy - 24 }, volatiles)
    -- Handled in FusionP
    FusionS _ -> return (state, volatiles)
    GFill _ _ -> do
      let allBots = botsPerformingGFill step
      let allCommands = (step M.!) <$> allBots
      guard $ all (\(GFill nd' fd') -> validNearDifference nd' && validFarDifference fd') allCommands

      let botPositions' = botPos <$> (stateBots M.!) <$> allBots
      
      let srcCorners = map (\((GFill nd' _), pos) -> pos + nd') $ zip allCommands botPositions'
      let dstCorners = map (\((GFill nd' fd'), pos) -> pos + nd' + fd') $ zip allCommands botPositions'
      guard $ all (\c -> T3.inBounds stateMatrix c) srcCorners
      guard $ all (\c -> T3.inBounds stateMatrix c) dstCorners
      guard $ all (\c -> elem c srcCorners) dstCorners

      let bboxes = map (\(s,d) -> getBox s d) $ zip srcCorners dstCorners
      let distinctRegions = S.toList $ S.fromList bboxes
      guard $ not $ any id $ concat $ map (\pos -> map (\(b0,b1) -> inBox b0 b1 pos) distinctRegions) botPositions'

      foldM (updateRegion VoxelFill) (state, volatiles) $ map (\(b0,b1) -> boxIndices b0 b1) distinctRegions
    GVoid _ _ -> do
      let allBots = botsPerformingGVoid step
      let allCommands = (step M.!) <$> allBots
      guard $ all (\(GVoid nd' fd') -> validNearDifference nd' && validFarDifference fd') allCommands

      let botPositions' = botPos <$> (stateBots M.!) <$> allBots
      
      let srcCorners = map (\((GVoid nd' _), pos) -> pos + nd') $ zip allCommands botPositions'
      let dstCorners = map (\((GVoid nd' fd'), pos) -> pos + nd' + fd') $ zip allCommands botPositions'
      guard $ all (\c -> T3.inBounds stateMatrix c) srcCorners
      guard $ all (\c -> T3.inBounds stateMatrix c) dstCorners
      guard $ all (\c -> elem c srcCorners) dstCorners

      let bboxes = map (\(s,d) -> getBox s d) $ zip srcCorners dstCorners
      let distinctRegions = S.toList $ S.fromList bboxes
      guard $ not $ any id $ concat $ map (\pos -> map (\(b0,b1) -> inBox b0 b1 pos) distinctRegions) botPositions'

      foldM (updateRegion VoxelVoid) (state, volatiles) $ map (\(b0,b1) -> boxIndices b0 b1) distinctRegions

  where botState = stateBots M.! botIdx
        myPos = botPos botState
        updateEnergyDelta action occupied = case action of
                                              VoxelFill -> if occupied then 6 else 12
                                              VoxelVoid -> if occupied then -12 else 3
        updateRegion action (state, volatiles) voxels = do
          volatiles' <- addVolatiles state volatiles (S.fromList voxels)
          let energyDelta = sum $ (updateEnergyDelta action) <$> (stateMatrix T3.!) <$> voxels
          return (state {
              stateMatrix = T3.update stateMatrix $ zip voxels [(actionToBool action)..],
              stateEnergy = stateEnergy + energyDelta
            }, volatiles')

addVolatiles :: ExecState -> Set VolatileCoordinate -> Set VolatileCoordinate -> Maybe (Set VolatileCoordinate)
addVolatiles (ExecState {..}) volatiles newVolatiles
  | S.null (volatiles `S.intersection` newVolatiles)
    && all (not . (stateMatrix T3.!)) newVolatiles = Just $ volatiles `S.union` newVolatiles
  | otherwise = Nothing
