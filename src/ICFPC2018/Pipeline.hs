module ICFPC2018.Pipeline
  ( pipeline
  , moveToZero
  , sliceModel
  , bboxHeuristics
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Linear.V3 (V3(..))
import Control.Applicative
import Control.Monad

import ICFPC2018.Types
import ICFPC2018.Utils
import ICFPC2018.Simulation
import ICFPC2018.Pack
import ICFPC2018.Tensor3 (I3)
import qualified ICFPC2018.Tensor3 as T3

data SearchWay = Any | Path I3 deriving (Show, Eq)

firstBot :: BotIdx
firstBot = 1

secondBot :: BotIdx
secondBot = 2

pipeline :: Model -> Trace
pipeline model = spawnBots : firstMove : moves ++ endStep' ++ moveToZero' ++ [haltStep]
  where
    intensions = solve model $ snakeIdx (T3.size model)
    state = case
      let (V3 r _ _ ) = T3.size model
      in foldM stepState (initialState r) [spawnBots, firstMove] of
        Nothing -> error "pipeline: invalid state (firstMove)"
        Just st -> st
    (state', moves) = moveTrace state intensions
    endStep' = endStep state'
    state'' = case foldM stepState state' endStep' of
      Nothing -> error "pipeline: invalid state (endStep)"
      Just st -> st
    moveToZero' = moveToZero state''
    haltStep = IM.singleton firstBot Halt

spawnBots :: Step
spawnBots = IM.singleton firstBot $ Fission (V3 1 0 0) 0

firstMove :: Step
firstMove = IM.fromList [(firstBot, LMove (V3 0 1 0) (V3 0 0 1)), (secondBot, Flip)]

moveTrace :: ExecState -> Intensions -> (ExecState, Trace)
moveTrace state intensions = helper state intensions []
  where
    mergedMove st ints = case moveBots st ints of
      Nothing -> Nothing
      Just (commands, ints') -> Just (mergeCommands commands, ints')
    proceedState st tr = case foldM stepState st tr of
      Nothing -> error "moveTrace: invalid state"
      Just st' -> st'
    helper st ints tr = case mergedMove st ints of
      Nothing -> (st, tr)
      Just (curTrace, ints') -> helper (proceedState st' fillMove) ints' (tr ++ curTrace ++ fillMove)
        where
          st' = proceedState st curTrace
          fillMove = [fillLine st']

moveBots :: ExecState -> Intensions -> Maybe (IntMap [Command], Intensions)
moveBots state intensions = case getNextLine intensions of
  Nothing -> Nothing
  Just ((beginPos, endPos), xs) -> Just (IM.fromList [(firstBot, firstCommands), (secondBot, secondCommands)], xs)
    where
      bots = stateBots state
      beginPos' = beginPos + (V3 0 1 0)
      endPos' = endPos + (V3 0 1 (-1))
      firstCommands = packMove (botPos $ bots IM.! firstBot) beginPos'
      secondCommands = packMove (botPos $ bots IM.! secondBot) endPos'

mergeCommands :: IntMap [Command] -> Trace
mergeCommands commands = getZipList $ (\m v -> IM.insert secondBot v m) <$> (IM.singleton firstBot <$> firstCommands') <*> secondCommands'
  where
    firstCommands = commands IM.! firstBot
    secondCommands = commands IM.! secondBot
    firstIsShorter = length firstCommands < length secondCommands
    firstCommands' = if firstIsShorter
                     then ZipList $ firstCommands ++ (repeat Wait)
                     else ZipList firstCommands
    secondCommands' = if firstIsShorter
                      then ZipList secondCommands
                      else ZipList $ secondCommands ++ (repeat Wait)

fillLine :: ExecState -> Step
fillLine state = IM.fromList [(firstBot, firstCommand), (secondBot, secondCommand)]
  where
    bots = stateBots state
    firstPos = botPos $ bots IM.! firstBot
    secondPos = botPos $ bots IM.! secondBot
    beginPos = firstPos - (V3 0 1 0)
    endPos = secondPos - (V3 0 1 (-1))
    firstCommand | beginPos == endPos = Fill $ beginPos - firstPos
                 | otherwise = GFill (beginPos - firstPos) (endPos - beginPos)
    secondCommand | beginPos == endPos = Wait
                  | otherwise = GFill (endPos - secondPos) (beginPos - endPos)

endStep :: ExecState -> Trace
endStep state = prepareMoves ++ [fusionStep]
  where
    bots = stateBots state
    firstPos = botPos $ bots IM.! firstBot
    secondPos = botPos $ bots IM.! secondBot
    firstCommands = [Flip]
    nextCoord = V3 0 0 (-1)
    secondCommands = packMove secondPos $ firstPos + nextCoord
    prepareMoves = mergeCommands $ IM.fromList [(firstBot, firstCommands), (secondBot, secondCommands)]
    fusionStep = IM.fromList [(firstBot, FusionP nextCoord), (secondBot, FusionS (-nextCoord))]

moveToZero :: ExecState -> Trace
moveToZero state = IM.singleton firstBot <$> commands
  where
    bots = stateBots state
    pos = botPos $ bots IM.! firstBot
    commands = packMove pos 0

solve :: Model -> [I3] -> Intensions
solve model idxs = map (\v -> FillIdx v) $ filter (\idx -> model T3.! idx) idxs

getNextLine :: Intensions -> Maybe ((I3, I3), Intensions)
getNextLine [] = Nothing
getNextLine ((FillIdx firstIdx):xs) = helper Any firstIdx xs
  where
    helper _ lastIdx [] = Just ((firstIdx, lastIdx), [])
    helper Any lastIdx intensions@((FillIdx idx):ixs)
      | mlenDistance lastIdx idx == 1 = helper (Path (idx - lastIdx)) idx ixs
      | otherwise = Just ((firstIdx, lastIdx), intensions)
    helper way@(Path dir) lastIdx intensions@((FillIdx idx):ixs)
      | mlenDistance firstIdx lastIdx >= maxFD = Just ((firstIdx, lastIdx), intensions)
      | idx - lastIdx == dir = helper way idx ixs
      | otherwise = Just ((firstIdx, lastIdx), intensions)
    helper _ lastIdx intensions = Just ((firstIdx, lastIdx), intensions)
getNextLine _ = undefined

sliceModel :: Model -> T3.Axis -> [Model]
sliceModel model axis = map (\(begin, end) -> T3.sliceAxis model axis begin (end - 1)) boundings
  where
    (V3 xsz ysz zsz) = T3.size model
    axisSize T3.X = xsz
    axisSize T3.Y = ysz
    axisSize T3.Z = zsz
    sz = axisSize axis
    indices = [0, maxFD .. (sz - 2)] ++ [sz]
    boundings = zip (init indices) (tail indices)

bboxHeuristics :: Model -> Double
bboxHeuristics model = sm / sz
  where
    bbox = T3.boundingBox model id
    subModel = T3.slice model bbox
    modelSum :: Int
    modelSum = T3.nonzero subModel
    sm :: Double
    sm = fromIntegral $ modelSum
    sz :: Double
    sz = fromIntegral $ product $ T3.size subModel
