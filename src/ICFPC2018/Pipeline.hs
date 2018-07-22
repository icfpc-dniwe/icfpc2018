module ICFPC2018.Pipeline where

--import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Linear.V3 (V3(..))

import ICFPC2018.Types
import ICFPC2018.Utils
import ICFPC2018.Simulation
import ICFPC2018.Pack
import ICFPC2018.Tensor3 (I3)
import qualified ICFPC2018.Tensor3 as T3

data SearchWay = Any | Path I3 deriving (Show, Eq)

pipeline :: Model -> Trace
pipeline = undefined

spawnBots :: ExecState -> Maybe ExecState
spawnBots state = stepState state $ M.singleton 0 $ Fission (V3 1 0 0) 2

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
      | mlenMetric lastIdx idx == 1 = helper (Path (idx - lastIdx)) idx ixs
      | otherwise = Just ((firstIdx, lastIdx), intensions)
    helper way@(Path dir) lastIdx intensions@((FillIdx idx):ixs)
      | mlenMetric firstIdx lastIdx >= maxFD = Just ((firstIdx, lastIdx), intensions)
      | idx - lastIdx == dir = helper way idx ixs
      | otherwise = Just ((firstIdx, lastIdx), intensions)
    helper _ lastIdx intensions = Just ((firstIdx, lastIdx), intensions)
getNextLine _ = undefined
