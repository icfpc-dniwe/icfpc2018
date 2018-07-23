module ICFPC2018.Simulation
  ( ExecStateData(..)
  , BotState(..)
  , ExecState(..)
  , MExecState
  , initialState
  , debugState
  , freezeState
  , thawState
  , stepState
  , stepStateM
  , reverseTrace
  ) where

import Data.Maybe
import Control.Monad
import Control.Arrow
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import qualified Data.Vector.Unboxed as V
import Control.Monad.Trans.Class
import Control.Monad.State.Strict hiding (state, execState)
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Linear.V3 (V3(..))
import GHC.Generics (Generic)
import Control.DeepSeq
import Control.Monad.ST
import Control.Monad.Loops

import ICFPC2018.Types
import ICFPC2018.Utils
import ICFPC2018.Tensor3 (I3)
import ICFPC2018.Tensor3 (Tensor3Size)
import qualified ICFPC2018.Tensor3 as T3
import ICFPC2018.Validation

import Debug.Trace

data ExecStateData = ExecStateData { stateEnergy :: !Int
                                   , stateHarmonics :: !HarmonicState
                                   , stateBots :: !(IntMap BotState)
                                   , stateHalted :: !Bool
                                   }
                   deriving (Show, Eq, Generic)

instance NFData ExecStateData

data ExecState = ExecState { stateData :: !ExecStateData
                           , stateMatrix :: !Model
                           }
               deriving (Show, Eq, Generic)

data MExecState s = MExecState !ExecStateData !(MModel s)

freezeState :: MExecState s -> ST s ExecState
freezeState (MExecState state modelM) = ExecState state <$> T3.freeze modelM

thawState :: ExecState -> ST s (MExecState s)
thawState (ExecState state model ) = MExecState state <$> T3.thaw model

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
initialState r = ExecState (ExecStateData { stateEnergy = 0
                                          , stateHarmonics = Low
                                          , stateBots = IM.singleton 1 initialBot
                                          , stateHalted = False
                                          })
                           (T3.create (V.replicate (product size) False) size)
  where initialBot = BotState { botPos = 0
                              , botSeeds = IS.fromList [2..20]
                              }
        size = V3 r r r

type SimulationST s a = ReaderT (MModel s, Tensor3Size) (StateT (ExecStateData, Set VolatileCoordinate) (MaybeT (ST s))) a

liftSim :: ST s a -> SimulationST s a
liftSim = lift . lift . lift

debugState :: ExecState -> Step -> Maybe ExecState
debugState state step =
  case stepState state step of
    Just state' -> Just state'
    Nothing -> traceShow (state, step) Nothing

stepState :: ExecState -> Step -> Maybe ExecState
stepState execState step = runST $ do
  execStateM <- thawState execState
  ret <- stepStateM execStateM step
  case ret of
    Nothing -> return Nothing
    Just (MExecState state modelM) -> do
      model <- T3.unsafeFreeze modelM
      return $ Just $ ExecState state model

stepStateM :: MExecState s -> Step -> ST s (Maybe (MExecState s))
stepStateM (MExecState state@(ExecStateData {..}) modelM) step = runMaybeT $ do
  size <- T3.mSize modelM
  (state', _) <- execStateT (runReaderT (stepStateSM step) (modelM, size)) (state, S.empty)
  return $ MExecState state' modelM

stepStateSM :: Step -> SimulationST s ()
stepStateSM step = do
  (ExecStateData {..}, _) <- get
  (_, size) <- ask
  guard $ not stateHalted
  guard $ IM.keys step == IM.keys stateBots
  let harmonicsCost = (if stateHarmonics == Low then 3 else 30) * product size
      botsCost = 20 * IM.size stateBots
      botPositions = M.fromList $ map (\(idx, bot) -> (botPos bot, idx)) $ IM.toList stateBots
  modify $ \(state, _) -> (state { stateEnergy = stateEnergy + harmonicsCost + botsCost }, M.keysSet botPositions)
  mapM_ (stepBot botPositions step) $ IM.toList step
  stepGroupBuilds step
  -- FIXME: check connectivity

stepGroupBuilds :: Step -> SimulationST s ()
stepGroupBuilds step = do
  stepGroupBuild step VoxelFill (\case GFill nd fd -> Just (nd, fd); _ -> Nothing)
  stepGroupBuild step VoxelVoid (\case GVoid nd fd -> Just (nd, fd); _ -> Nothing)

stepGroupBuild :: Step -> SimulationVoxelAction -> (Command -> Maybe (NearDifference, FarDifference)) -> SimulationST s ()
stepGroupBuild step action extract = do
  (ExecStateData {..}, _) <- get
  (_, size) <- ask
  case mapMaybe (\(idx, cmd) -> (botPos $ stateBots IM.! idx, ) <$> extract cmd) $ IM.toList step of
    [] -> return ()
    commands -> do
      guard $ all (\(_, (nd', fd')) -> validNearDifference nd' && validFarDifference fd') commands

      let srcCorners = map (\(pos, (nd', _)) -> pos + nd') commands
      let dstCorners = map (\(pos, (nd', fd')) -> pos + nd' + fd') commands

      guard $ all (\c -> inSizeBounds size c) srcCorners
      guard $ all (\c -> inSizeBounds size c) dstCorners
      guard $ all (\c -> elem c srcCorners) dstCorners

      let bboxes = map (\(s,d) -> getBox s d) $ zip srcCorners dstCorners
      let distinctRegions = S.toList $ S.fromList bboxes

      guard $ not $ any id $ concat $ map (\(pos, _) -> map (\(b0,b1) -> inBox b0 b1 pos) distinctRegions) commands

      mapM_ (updateRegion action) $ map (\(b0, b1) -> boxIndices b0 b1) distinctRegions

stepBot :: BotPositions -> Step -> (BotIdx, Command) -> SimulationST s ()
stepBot botPositions step (botIdx, command) = do
  (ExecStateData {..}, _) <- get
  (_, size) <- ask
  let botState = stateBots IM.! botIdx
      myPos = botPos botState
  
  case command of
    Halt -> do
      guard $ IM.size stateBots == 1 && myPos == 0
      modify $ first $ \state -> state { stateHalted = True }
    Wait -> return ()
    Flip -> modify $ first $ \state -> state { stateHarmonics = changeHarmonic stateHarmonics }
    SMove lld -> do
      let newPos = myPos + lld
      guard $ validLongDifference lld && inSizeBounds size newPos
      addFreeVolatiles $ S.fromList $ linearPath myPos lld
      let newBots = IM.insert botIdx (botState { botPos = newPos }) stateBots
      modify $ first $ \state -> state { stateBots = newBots, stateEnergy = stateEnergy + 2 * mlen lld }
    LMove sld1 sld2 -> do
      let cornerPos = myPos + sld1
          newPos = cornerPos + sld2
      guard $ validShortDifference sld1 && validShortDifference sld2 && inSizeBounds size cornerPos && inSizeBounds size newPos
      addFreeVolatiles $ S.fromList $ linearPath myPos sld1 ++ linearPath cornerPos sld2
      let newBots = IM.insert botIdx (botState { botPos = newPos }) stateBots
      modify $ first $ \state -> state { stateBots = newBots, stateEnergy = stateEnergy + 2 * (clen sld1 + clen sld2 + 2) }
    Fill nd -> do
      let pos = myPos + nd
      guard $ validNearDifference nd && fillablePoint size pos
      updateRegion VoxelFill [pos]
    Void nd -> do
      let pos = myPos + nd
      guard $ validNearDifference nd && fillablePoint size pos
      updateRegion VoxelVoid [pos]
    Fission nd m -> do
      let (childId:childSeeds, parentSeeds) = splitAt (m + 1) $ IS.toAscList $ botSeeds botState
          childPos = myPos + nd
      guard $ validNearDifference nd && inSizeBounds size childPos
      addFreeVolatiles $ S.singleton childPos
      let newBots = IM.insert botIdx (botState { botSeeds = IS.fromList parentSeeds }) $
                    IM.insert childId (BotState { botPos = childPos
                                                , botSeeds = IS.fromList childSeeds
                                                }) $
                    stateBots
      modify $ first $ \state -> state { stateBots = newBots, stateEnergy = stateEnergy + 24 }
    FusionP nd -> do
      let childPos = myPos + nd
      guard $ validNearDifference nd && inSizeBounds size childPos
      childIdx <- maybeAlt $ M.lookup childPos botPositions
      let FusionS childNd = step IM.! childIdx
      guard $ childPos + childNd == myPos
      let childState = stateBots IM.! botIdx
      let newBots = IM.insert botIdx (botState { botSeeds = IS.insert childIdx $ botSeeds botState `IS.union` botSeeds childState }) $
                    IM.delete childIdx stateBots
      modify $ first $ \state -> state { stateBots = newBots, stateEnergy = stateEnergy - 24 }
    -- Handled in FusionP
    FusionS _ -> return ()
    -- Handled in stepState
    GFill _ _ -> return ()
    GVoid _ _ -> return ()

  where addFreeVolatiles newVolatiles = do
          (modelM, _) <- ask
          occupied <- liftSim $ anyM (T3.read modelM) $ S.toList newVolatiles
          guard $ not occupied
          (_, volatiles) <- get
          volatiles' <- maybeAlt $ addVolatiles volatiles newVolatiles
          modify $ second $ const volatiles'

updateRegion :: forall s. SimulationVoxelAction -> [I3] -> SimulationST s ()
updateRegion action voxels = do
  (modelM, _) <- ask :: SimulationST s (MModel s, Tensor3Size)
  (ExecStateData {..}, volatiles) <- get
  volatiles' <- maybeAlt $ addVolatiles volatiles $ S.fromList voxels
  energyDelta <- liftSim $ fmap sum $ mapM ((pure . updateEnergyDelta action) <=< T3.read modelM) voxels
  liftSim $ mapM_ (uncurry $ T3.write modelM) $ zip voxels (repeat $ actionToBool action);;
  modify $ \(state, _) -> (state { stateEnergy = stateEnergy + energyDelta }, volatiles')

  where updateEnergyDelta VoxelFill True = 6
        updateEnergyDelta VoxelFill False = 12
        updateEnergyDelta VoxelVoid True = -12
        updateEnergyDelta VoxelVoid False = 3

addVolatiles :: Set VolatileCoordinate -> Set VolatileCoordinate -> Maybe (Set VolatileCoordinate)
addVolatiles volatiles newVolatiles
  | S.null (volatiles `S.intersection` newVolatiles) = Just $ volatiles `S.union` newVolatiles
  | otherwise = Nothing

reverseTrace :: Int -> Trace -> Trace
reverseTrace r trace0 = reverse $ (IM.singleton 1 Halt):trace' where

  trace' = evalState (mapM reverseTraceStep trace0) (initialState r)

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
      idx' = IS.findMin $ botSeeds ((stateBots $ stateData s) IM.! idx)
      in [(idx, FusionP d), (idx', FusionS (-d))]

    (Fill d)      -> pure $ (idx, Void d)
    (Void d)      -> pure $ (idx, Fill d)

    (FusionP d)   -> let
      bot  = (stateBots $ stateData s) IM.! idx
      bot' = head . IM.elems . IM.filter ((== d + (botPos bot)) . botPos) $ (stateBots $ stateData s)
      in pure (idx, Fission d (IS.size (botSeeds bot)))

    (FusionS d)   -> []
    (GFill d1 d2) -> pure $ (idx, GVoid d1 d2)
    (GVoid d1 d2) -> pure $ (idx, GFill d1 d2)
