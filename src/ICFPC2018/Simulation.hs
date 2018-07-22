module ICFPC2018.Simulation
  ( ExecState
  , stateEnergy
  , stateHarmonics
  , stateMatrix
  , stateBots
  , stateHalted
  , BotState
  , botPos
  , botSeeds
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

initialState :: Model -> ExecState
initialState model = ExecState { stateEnergy = 0
                               , stateHarmonics = Low
                               , stateMatrix = model
                               , stateBots = M.singleton 1 initialBot
                               , stateHalted = False
                               }
  where initialBot = BotState { botPos = 0
                              , botSeeds = IS.fromList [2..20]
                              }

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
      let newPos = myPos + sld1 + sld2
      guard $ validShortDifference sld1 && validShortDifference sld2 && T3.inBounds stateMatrix newPos
      volatiles' <- addVolatiles state volatiles (S.fromList $ linearPath myPos sld1 ++ linearPath (myPos + sld1) sld2)
      let newBots = M.insert botIdx (botState { botPos = newPos }) stateBots
      return (state { stateBots = newBots, stateEnergy = stateEnergy + 2 * (clen sld1 + clen sld2 + 2) }, volatiles')
    Fill nd -> do
      let pos = myPos + nd
          curr = stateMatrix T3.! pos
      guard $ validNearDifference nd && T3.inBounds stateMatrix pos
      volatiles' <- addVolatiles state volatiles (S.singleton pos)
      return (state { stateMatrix = T3.update stateMatrix [(pos, True)], stateEnergy = stateEnergy + (if curr then 6 else 12) }, volatiles')
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
  where botState = stateBots M.! botIdx
        myPos = botPos botState

addVolatiles :: ExecState -> Set VolatileCoordinate -> Set VolatileCoordinate -> Maybe (Set VolatileCoordinate)
addVolatiles (ExecState {..}) volatiles newVolatiles
  | S.null (volatiles `S.intersection` newVolatiles)
    && all (not . (stateMatrix T3.!)) newVolatiles = Just $ volatiles `S.union` newVolatiles
  | otherwise = Nothing
