module ICFPC2018.Simulation
  ( ExecState(..)
  , BotState(..)
  , initialState
  , debugState
  , stepState
  ) where

import Control.Monad
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import qualified Data.Vector.Unboxed as V
import Linear.V3 (V3(..))
import Debug.Trace
import GHC.Generics (Generic)
import Control.DeepSeq
import Control.Monad.State.Strict
import Data.Maybe (fromMaybe)

import ICFPC2018.Types
import ICFPC2018.Utils
import ICFPC2018.Tensor3 (I3)
import qualified ICFPC2018.Tensor3 as T3
import ICFPC2018.Validation

import Debug.Trace

data ExecState = ExecState { stateEnergy :: !Int
                           , stateHarmonics :: !HarmonicState
                           , stateMatrix :: !Model
                           , stateBots :: !(IntMap BotState)
                           , stateGFillDone :: !Bool
                           , stateGVoidDone :: !Bool
                           , stateHalted :: !Bool
                           }
                           deriving (Show, Eq, Generic)

instance NFData ExecState

type SeedsSet = IntSet

type BotPositions = Map I3 BotIdx

data FusionStatus = FusionMaster | FusionSlave
                  deriving (Show, Eq)

data BotState = BotState { botPos :: !I3
                         , botSeeds :: !SeedsSet
                         }
              deriving (Show, Eq, Generic)

instance NFData BotState

data SimulationVoxelAction = VoxelFill | VoxelVoid
                           deriving (Show, Eq)
actionToBool :: SimulationVoxelAction -> Bool
actionToBool VoxelFill = True
actionToBool VoxelVoid = False

initialState :: Int -> ExecState
initialState r = ExecState { stateEnergy = 0
                           , stateHarmonics = Low
                           , stateMatrix = T3.create (V.replicate (product size) False) size
                           , stateBots = IM.singleton 1 initialBot
                           , stateGFillDone = False
                           , stateGVoidDone = False
                           , stateHalted = False
                           }
  where initialBot = BotState { botPos = 0
                              , botSeeds = IS.fromList [2..20]
                              }
        size = V3 r r r

debugState :: ExecState -> Step -> Maybe ExecState
debugState state step =
  case stepState state step of
    Just state' -> Just state'
    Nothing -> traceShow (state, step) Nothing

stepState :: ExecState -> Step -> Maybe ExecState
stepState state@(ExecState {..}) step = do
  guard $ not stateHalted
  guard $ IM.keys step == IM.keys stateBots
  let harmonicsCost = (if stateHarmonics == Low then 3 else 30) * product (T3.size stateMatrix)
      botsCost = 20 * IM.size stateBots
      state1 = state { stateEnergy = stateEnergy + harmonicsCost + botsCost, stateGFillDone = False, stateGVoidDone = False }
  let botPositions = M.fromList $ map (\(idx, bot) -> (botPos bot, idx)) $ IM.toList stateBots
  (state2, _) <- foldM (stepBot botPositions step) (state1, M.keysSet botPositions) $ IM.toList step
  -- FIXME: check connectivity
  return state2

botsPerformingCommand :: Step -> (Command -> Bool) -> [BotIdx]
botsPerformingCommand step pr = IM.keys $ IM.filter pr step

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
      guard $ IM.size stateBots == 1 && myPos == 0
      return (state { stateHalted = True }, volatiles)
    Wait -> Just (state, volatiles)
    Flip -> Just (state { stateHarmonics = changeHarmonic stateHarmonics }, volatiles)
    SMove lld -> do
      let newPos = myPos + lld
      guard $ validLongDifference lld && T3.inBounds stateMatrix newPos
      volatiles' <- addFreeVolatiles $ S.fromList $ linearPath myPos lld
      let newBots = IM.insert botIdx (botState { botPos = newPos }) stateBots
      return (state { stateBots = newBots, stateEnergy = stateEnergy + 2 * mlen lld }, volatiles')
    LMove sld1 sld2 -> do
      let cornerPos = myPos + sld1
          newPos = cornerPos + sld2
      guard $ validShortDifference sld1 && validShortDifference sld2 && T3.inBounds stateMatrix cornerPos && T3.inBounds stateMatrix newPos
      volatiles' <- addFreeVolatiles $ S.fromList $ linearPath myPos sld1 ++ linearPath cornerPos sld2
      let newBots = IM.insert botIdx (botState { botPos = newPos }) stateBots
      return (state { stateBots = newBots, stateEnergy = stateEnergy + 2 * (clen sld1 + clen sld2 + 2) }, volatiles')
    Fill nd -> do
      let pos = myPos + nd
      guard $ validNearDifference nd && fillablePoint stateMatrix pos
      updateRegion VoxelFill (state, volatiles) [pos]
    Void nd -> do
      let pos = myPos + nd
      guard $ validNearDifference nd && fillablePoint stateMatrix pos
      updateRegion VoxelVoid (state, volatiles) [pos]
    Fission nd m -> do
      let (childId:childSeeds, parentSeeds) = splitAt (m + 1) $ IS.toAscList $ botSeeds botState
          childPos = myPos + nd
      guard $ validNearDifference nd && T3.inBounds stateMatrix childPos
      volatiles' <- addFreeVolatiles $ S.singleton childPos
      let newBots = IM.insert botIdx (botState { botSeeds = IS.fromList parentSeeds }) $
                    IM.insert childId (BotState { botPos = childPos
                                                , botSeeds = IS.fromList childSeeds
                                                }) $
                    stateBots
      return (state { stateBots = newBots, stateEnergy = stateEnergy + 24 }, volatiles')
    FusionP nd -> do
      let childPos = myPos + nd
      guard $ validNearDifference nd && T3.inBounds stateMatrix childPos
      childIdx <- M.lookup childPos botPositions
      let FusionS childNd = step IM.! childIdx
      guard $ childPos + childNd == myPos
      let childState = stateBots IM.! botIdx
      let newBots = IM.insert botIdx (botState { botSeeds = IS.insert childIdx $ botSeeds botState `IS.union` botSeeds childState }) $
                    IM.delete childIdx stateBots
      return (state { stateBots = newBots, stateEnergy = stateEnergy - 24 }, volatiles)
    -- Handled in FusionP
    FusionS _ -> return (state, volatiles)
    GFill _ _ -> do
      traceM ("state gfill: " ++ show stateGFillDone)

      if (not stateGFillDone) then do
        let allBots = botsPerformingGFill step
        let allCommands = (step IM.!) <$> allBots
        let botPositions' = botPos <$> (stateBots IM.!) <$> allBots

        traceM ("bots       : " ++ show allBots)
        traceM ("positions  : " ++ show botPositions')
        traceM ("commands   : " ++ show allCommands)
        traceM ("check      : " ++ show (map (\(GFill nd' fd') -> validNearDifference nd' && validFarDifference fd') allCommands))
        guard $ all (\(GFill nd' fd') -> validNearDifference nd' && validFarDifference fd') allCommands

        let srcCorners = map (\((GFill nd' _), pos) -> pos + nd') $ zip allCommands botPositions'
        let dstCorners = map (\((GFill nd' fd'), pos) -> pos + nd' + fd') $ zip allCommands botPositions'

        traceM ("src corners: " ++ show srcCorners)
        traceM ("dst corners: " ++ show dstCorners)
        guard $ all (\c -> T3.inBounds stateMatrix c) srcCorners
        guard $ all (\c -> T3.inBounds stateMatrix c) dstCorners
        guard $ all (\c -> elem c srcCorners) dstCorners

        let bboxes = map (\(s,d) -> getBox s d) $ zip srcCorners dstCorners
        let distinctRegions = S.toList $ S.fromList bboxes

        traceM ("distinctRegions: " ++ show distinctRegions)
        traceM ("check2         : " ++ show (not $ any id $ concat $ map (\pos -> map (\(b0,b1) -> inBox b0 b1 pos) distinctRegions) botPositions'))
        guard $ not $ any id $ concat $ map (\pos -> map (\(b0,b1) -> inBox b0 b1 pos) distinctRegions) botPositions'

        (state', volatiles') <- foldM (updateRegion VoxelFill) (state, volatiles) $ map (\(b0,b1) -> boxIndices b0 b1) distinctRegions
        return (state' { stateGFillDone = True }, volatiles')
      else do
        return (state, volatiles)
    GVoid _ _ -> do
      if (not stateGVoidDone) then do
        let allBots = botsPerformingGVoid step
        let allCommands = (step IM.!) <$> allBots
        guard $ all (\(GVoid nd' fd') -> validNearDifference nd' && validFarDifference fd') allCommands

        let botPositions' = botPos <$> (stateBots IM.!) <$> allBots

        let srcCorners = map (\((GVoid nd' _), pos) -> pos + nd') $ zip allCommands botPositions'
        let dstCorners = map (\((GVoid nd' fd'), pos) -> pos + nd' + fd') $ zip allCommands botPositions'
        guard $ all (\c -> T3.inBounds stateMatrix c) srcCorners
        guard $ all (\c -> T3.inBounds stateMatrix c) dstCorners
        guard $ all (\c -> elem c srcCorners) dstCorners

        let bboxes = map (\(s,d) -> getBox s d) $ zip srcCorners dstCorners
        let distinctRegions = S.toList $ S.fromList bboxes
        guard $ not $ any id $ concat $ map (\pos -> map (\(b0,b1) -> inBox b0 b1 pos) distinctRegions) botPositions'

        (state', volatiles') <- foldM (updateRegion VoxelVoid) (state, volatiles) $ map (\(b0,b1) -> boxIndices b0 b1) distinctRegions
        return $ (state' { stateGVoidDone = True }, volatiles')
      else do
        return (state, volatiles)

  where botState = stateBots IM.! botIdx
        myPos = botPos botState
        updateEnergyDelta action occupied = case action of
                                              VoxelFill -> if occupied then 6 else 12
                                              VoxelVoid -> if occupied then -12 else 3
        updateRegion action (state'', volatiles'') voxels = do
          --traceM ("updateRegion: add volatiles: " ++ show (state'', volatiles''))
          volatiles' <- addVolatiles volatiles'' (S.fromList voxels)
          let energyDelta = sum $ (updateEnergyDelta action) <$> (stateMatrix T3.!) <$> voxels
          return (state'' {
              stateMatrix = T3.update stateMatrix $ zip voxels [(actionToBool action)..],
              stateEnergy = stateEnergy + energyDelta
            }, volatiles')
        addFreeVolatiles newVolatiles = do
          guard $ all (not . (stateMatrix T3.!)) newVolatiles
          addVolatiles volatiles newVolatiles

addVolatiles :: Set VolatileCoordinate -> Set VolatileCoordinate -> Maybe (Set VolatileCoordinate)
addVolatiles volatiles newVolatiles
  | S.null (volatiles `S.intersection` newVolatiles) = Just $ volatiles `S.union` newVolatiles
  | otherwise = Nothing



reverseTrace :: Int -> Trace -> Trace
reverseTrace r trace = reverse $ (IM.singleton 1 Halt):trace' where

  trace' = evalState (mapM reverseTraceStep trace) (initialState r)

  reverseTraceStep :: Step -> State ExecState Step
  reverseTraceStep step = do
    state <- get
    let state' = fromMaybe (error "reverseTrace: stepState returned Nothing") (stepState state step)
    let step'  = IM.fromList $ concatMap (uncurry $ reverseCommand (state, state')) $ IM.toList step
    put state'
    return step'


  reverseCommand :: (ExecState, ExecState) -> BotIdx -> Command -> [(BotIdx, Command)]
  reverseCommand (s, s') idx = \case
    Halt          -> []
    Wait          -> pure $ (idx, Wait)
    Flip          -> pure $ (idx, Flip)
    (SMove d)     -> pure $ (idx, SMove (-d))
    (LMove d1 d2) -> pure $ (idx, LMove (-d2) (-d1))

    (Fission d m) -> let
      idx' = IS.findMin $ botSeeds ((stateBots s) IM.! idx)
      in [(idx, FusionP d), (idx', FusionS (-d))]

    (Fill d)      -> pure $ (idx, Void d)
    (Void d)      -> pure $ (idx, Fill d)

    (FusionP d)   -> let
      bot  = (stateBots s) IM.! idx
      bot' = head . IM.elems . IM.filter ((== d + (botPos bot)) . botPos) $ (stateBots s)
      in pure (idx, Fission d (IS.size (botSeeds bot)))

    (FusionS d)   -> []
    (GFill d1 d2) -> pure $ (idx, GVoid d1 d2)
    (GVoid d1 d2) -> pure $ (idx, GFill d1 d2)
