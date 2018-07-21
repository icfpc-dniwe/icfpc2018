module ICFPC2018.Model
  ( neighbours
  , showLayer
  , aStar
  ) where

import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as PQ
import Linear.V3 (V3(..))

import ICFPC2018.Tensor3 (Tensor3, I3)
import qualified ICFPC2018.Tensor3 as T3
import ICFPC2018.Types
import ICFPC2018.Utils

neighbours :: I3 -> Model -> [I3]
neighbours p model = filter (\i -> checkBounds (T3.size model) i && not (model T3.! i)) $ map (p +) allNeighbours
  where allNeighbours =
          [ V3 (-1) 0    0
          , V3 1    0    0
          , V3 0    (-1) 0
          , V3 0    1    0
          , V3 0    0    (-1)
          , V3 0    0    1
          ]

showLayer :: Int -> Model -> String
showLayer y model = intercalate "\n" $ map showLine [0..zS-1]
  where V3 xS _ zS = T3.size model
        showLine z = map (\x -> if model T3.! V3 x y z then '#' else ' ') [0..xS-1]

aStar :: I3 -> I3 -> Model -> Maybe [I3]
aStar start finish model = go (PQ.singleton (mlen (finish - start)) start) S.empty M.empty (M.singleton start 0)
  where go :: MinPQueue Int I3 -> Set I3 -> Map I3 I3 -> Map I3 Int -> Maybe [I3]
        go queue0 closedSet0 cameFrom0 gScore0 =
          case PQ.minView queue0 of
            Nothing -> Nothing
            Just (current, queue0')
              | current == finish -> Just $ traverseBack cameFrom0 finish []
              | current `S.member` closedSet0 -> go queue0' closedSet0 cameFrom0 gScore0
              | otherwise -> go queue1 (S.insert current closedSet0) cameFrom1 gScore1
              where currentScore = gScore0 M.! current
                    (queue1, cameFrom1, gScore1) = foldr lookNeighbour (queue0', cameFrom0, gScore0) $ neighbours current model

                    lookNeighbour :: I3 -> (MinPQueue Int I3, Map I3 I3, Map I3 Int) -> (MinPQueue Int I3, Map I3 I3, Map I3 Int)
                    lookNeighbour neighbour (queue, cameFrom, gScore) =
                      case M.lookup neighbour gScore of
                        Just score | score <= tentativeScore -> (queue, cameFrom, gScore)
                        _ -> (queue', cameFrom', gScore')
                      where tentativeScore = currentScore + mlen (neighbour - current)
                            queue' = PQ.insert tentativeScore neighbour queue
                            cameFrom' = M.insert neighbour current cameFrom
                            gScore' = M.insert neighbour tentativeScore gScore
        traverseBack cameFrom current path
          | current == start = current : path
          | otherwise = traverseBack cameFrom (cameFrom M.! current) (current : path)
