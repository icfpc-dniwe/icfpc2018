module ICFPC2018.Pipeline where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
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
pipeline model = spawnBots : firstMove : moves ++ endStep' ++ moveToZero'
  where
    intensions = solve model $ snakeIdx (T3.size model)
    state = case let (V3 r _ _ ) = T3.size model in foldM stepState (initialState r) [spawnBots, firstMove] of
      Nothing -> error "pipeline: invalid state (firstMove)"
      Just st -> st
    (state', moves) = moveTrace state intensions
    endStep' = endStep state'
    state'' = case foldM stepState state' endStep' of
      Nothing -> error "pipeline: invalid state (endStep)"
      Just st -> st
    moveToZero' = moveToZero state''

spawnBots :: Step
spawnBots = M.singleton firstBot $ Fission (V3 1 0 0) secondBot

firstMove :: Step
firstMove = M.fromList [(firstBot, SMove (V3 0 1 0)), (secondBot, Flip)]

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
      Just (curTrace, ints') -> helper (proceedState st curTrace) ints' (tr ++ curTrace)

moveBots :: ExecState -> Intensions -> Maybe (Map BotIdx [Command], Intensions)
moveBots state intensions = case getNextLine intensions of
  Nothing -> Nothing
  Just ((beginPos, endPos), xs) -> Just (M.fromList [(firstBot, firstCommands), (secondBot, secondCommands)], xs)
    where
      bots = stateBots state
      beginPos' = beginPos + (V3 0 1 0)
      endPos' = endPos + (V3 0 1 (-1))
      firstCommands = packMove (botPos $ bots M.! firstBot) beginPos'
      secondCommands = packMove (botPos $ bots M.! secondBot) endPos'

mergeCommands :: Map BotIdx [Command] -> Trace
mergeCommands commands = getZipList $ (\m v -> M.insert secondBot v m) <$> (M.singleton firstBot <$> firstCommands') <*> secondCommands'
  where
    firstCommands = commands M.! firstBot
    secondCommands = commands M.! secondBot
    firstIsShorter = length firstCommands < length secondCommands
    firstCommands' = if firstIsShorter
                     then ZipList $ firstCommands ++ (repeat Wait)
                     else ZipList firstCommands
    secondCommands' = if firstIsShorter
                      then ZipList secondCommands
                      else ZipList $ secondCommands ++ (repeat Wait)

fillLine :: ExecState -> Step
fillLine state = M.fromList [(firstBot, GFill beginPos endPos), (secondBot, GFill endPos beginPos)]
  where
    bots = stateBots state
    firstPos = botPos $ bots M.! firstBot
    secondPos = botPos $ bots M.! secondBot
    beginPos = firstPos - (V3 0 1 0)
    endPos = secondPos - (V3 0 1 (-1))

endStep :: ExecState -> Trace
endStep state = prepareMoves ++ [fusionStep]
  where
    bots = stateBots state
    firstPos = botPos $ bots M.! firstBot
    secondPos = botPos $ bots M.! secondBot
    firstCommands = [Flip]
    nextCoord = V3 0 0 (-1)
    secondCommands = packMove secondPos $ firstPos + nextCoord
    prepareMoves = mergeCommands $ M.fromList [(firstBot, firstCommands), (secondBot, secondCommands)]
    fusionStep = M.fromList [(firstBot, FusionP nextCoord), (secondBot, FusionS (-nextCoord))]

moveToZero :: ExecState -> Trace
moveToZero state = M.singleton firstBot <$> commands
  where
    bots = stateBots state
    pos = botPos $ bots M.! firstBot
    commands = packMove pos 0
{-
sliceModel :: Model -> Int -> [T3.BoundingBox]
sliceModel m0 numBots = undefined
  where
    modelBox = T3.boundingBox m0 id
    bottomLine = case modelBox of
      (V3 x1 y1 z1, V3 x2 y2 z2) -> if (x2 - x1) > (z2 - z1)
                                    then (X, x1, x2)
                                    else (Z, z1, z2)
    slice (X, from, to) = undefined
    slice (Z, from, to) = undefined

    mergeTraces :: [Trace] -> Trace
    mergeTraces traces = foldr helper $ zip [1..] traces
      where
        helper = undefined
-}

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
