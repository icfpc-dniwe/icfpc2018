module ICFPC2018.Simulation where

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

data State = State { stateEnergy :: !Int
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

type BotFusions = Map BotIdx (FusionStatus, BotIdx)

data BotState = BotState { botPos :: !I3
                         , botSeeds :: !SeedsSet
                         }
              deriving (Show, Eq)

initialState :: Model -> State
initialState model = State { stateEnergy = 0
                           , stateHarmonics = Low
                           , stateMatrix = model
                           , stateBots = M.singleton 1 initialBot
                           , stateHalted = False
                           }
  where initialBot = BotState { botPos = 0
                              , botSeeds = IS.fromList [2..20]
                              }

stepState :: State -> Step -> Maybe State
stepState state@(State {..}) step = do
  guard $ not stateHalted
  guard $ M.keys step == M.keys stateBots
  let harmonicsCost = (if stateHarmonics == Low then 3 else 30) * product (T3.size stateMatrix)
      botsCost = 20 * M.size stateBots
      state1 = state { stateEnergy = stateEnergy + harmonicsCost + botsCost }
  let botPositions = M.fromList $ map (\(idx, bot) -> (botPos bot, idx)) $ M.toList stateBots
  (state2, _) <- foldM (stepBot botPositions step) (state1, M.keysSet botPositions) $ M.toList step
  -- FIXME: check connectivity
  return state2

stepBot :: BotPositions -> Step -> (State, Set VolatileCoordinate) -> (BotIdx, Command) -> Maybe (State, Set VolatileCoordinate)
stepBot botPositions step (state@State {..}, volatiles) (botIdx, command) =
  case command of
    Halt -> do
      guard $ M.size stateBots == 1 && myPos == 0
      return (state { stateHalted = True }, volatiles)
    Wait -> Just (state, volatiles)
    Flip -> Just (state { stateHarmonics = changeHarmonic stateHarmonics }, volatiles)
    SMove lld -> do
      guard $ validLongDifference lld
      volatiles' <- addVolatiles state volatiles (S.fromList $ linearPath myPos lld)
      return ((insertBot $ botState { botPos = myPos + lld }) { stateEnergy = stateEnergy + 2 * mlen lld }, volatiles')
    LMove sld1 sld2 -> do
      guard $ validShortDifference sld1 && validShortDifference sld2
      volatiles' <- addVolatiles state volatiles (S.fromList $ linearPath myPos sld1 ++ linearPath (myPos + sld1) sld2)
      return ((insertBot $ botState { botPos = myPos + sld1 + sld2 }) { stateEnergy = stateEnergy + 2 * (clen sld1 + clen sld2 + 2) }, volatiles')
    Fill nd -> do
      guard $ validNearDifference nd
      let pos = myPos + nd
          curr = stateMatrix T3.! pos
      volatiles' <- addVolatiles state volatiles (S.singleton pos)
      return (state { stateMatrix = T3.update stateMatrix [(pos, True)], stateEnergy = stateEnergy + (if curr then 6 else 12) }, volatiles')
    Fission nd m -> do
      let (childId:childSeeds, parentSeeds) = splitAt (m + 1) $ IS.toAscList $ botSeeds botState
          childPos = myPos + nd
      guard $ validNearDifference nd
      volatiles' <- addVolatiles state volatiles (S.singleton childPos)
      let newBots = M.insert botIdx (botState { botSeeds = IS.fromList parentSeeds }) $
                    M.insert childId (BotState { botPos = childPos
                                               , botSeeds = IS.fromList childSeeds
                                               }) $
                    stateBots
      return (state { stateBots = newBots, stateEnergy = stateEnergy + 24 }, volatiles')
    FusionP nd -> do
      guard $ validNearDifference nd
      let childPos = myPos + nd
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
        insertBot botState' = state { stateBots = M.insert botIdx botState' stateBots }

linearPath :: I3 -> I3 -> [I3]
linearPath from path
  | path == 0 = []
  | otherwise = next : linearPath next (path - step)
  where step = signum path
        next = from + step
                                                      
addVolatiles :: State -> Set VolatileCoordinate -> Set VolatileCoordinate -> Maybe (Set VolatileCoordinate)
addVolatiles (State {..}) volatiles newVolatiles
  | S.null (volatiles `S.intersection` newVolatiles)
    && all (not . (stateMatrix T3.!)) newVolatiles = Just $ volatiles `S.union` newVolatiles
  | otherwise = Nothing
