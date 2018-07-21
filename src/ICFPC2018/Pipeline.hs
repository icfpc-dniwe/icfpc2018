module ICFPC2018.Pipeline where

import ICFPC2018.Types
import ICFPC2018.Simulations
import ICFPC2018.Tensor3 as T3

pipeline :: Model -> Trace
pipeline = undefined

spawnBots :: Int -> Trace
spawnBots = undefined

sliceModel :: Model -> Int -> [T3.BoundingBox]
sliceModel m0 numBots = undefined
  where
    modelBox = T3.boundingBox m0 id
    bottomLine = case modelBox of
      (V3 x1 y1 z1, V3 x2 y2 z2) -> if (x2 - x1) > (z2 - z1)
                                    then (X, x2 - x1)
                                    else (Z, z2 - z1)
    slice (X, xdiff) = undefined
    slice (Z, zdiff) = undefined

solve :: Model -> [T3.I3] -> Intensions
solve idxs = map (\v -> FillIdx v) $ filter (\idx -> model T3.! idx) idxs

mergeTraces :: [Trace] -> Trace
mergeTraces traces = foldr helper $ zip [1..] traces
  where
    helper = undefined
