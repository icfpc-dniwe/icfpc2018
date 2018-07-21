module ICFPC2018.Model
  ( showLayer
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

import qualified ICFPC2018.Tensor3 as T3
import ICFPC2018.Types

showLayer :: Int -> Model -> String
showLayer y model = intercalate "\n" $ map showLine [0..zS-1]
  where V3 xS _ zS = T3.size model
        showLine z = map (\x -> if model T3.! V3 x y z then '#' else ' ') [0..xS-1]

aStar :: forall i tag metric. (Ord i, Ord metric, Num metric) => (i -> [(i, tag)]) -> (i -> i -> metric) -> i -> i -> Maybe [(i, tag)]
aStar neighbours metric start finish = go (PQ.singleton (metric finish start) start) S.empty M.empty (M.singleton start 0)
  where go :: MinPQueue metric i -> Set i -> Map i (i, tag) -> Map i metric -> Maybe [(i, tag)]
        go queue0 closedSet0 cameFrom0 gScore0 =
          case PQ.minView queue0 of
            Nothing -> Nothing
            Just (current, queue0')
              | current == finish -> Just $ traverseBack cameFrom0 finish []
              | current `S.member` closedSet0 -> go queue0' closedSet0 cameFrom0 gScore0
              | otherwise -> go queue1 (S.insert current closedSet0) cameFrom1 gScore1
              where currentScore = gScore0 M.! current
                    (queue1, cameFrom1, gScore1) = foldr lookNeighbour (queue0', cameFrom0, gScore0) $ neighbours current

                    lookNeighbour :: (i, tag) -> (MinPQueue metric i, Map i (i, tag), Map i metric) -> (MinPQueue metric i, Map i (i, tag), Map i metric)
                    lookNeighbour (neighbour, tag) (queue, cameFrom, gScore) =
                      case M.lookup neighbour gScore of
                        Just score | score <= tentativeScore -> (queue, cameFrom, gScore)
                        _ -> (queue', cameFrom', gScore')
                      where tentativeScore = currentScore + metric neighbour current
                            queue' = PQ.insert tentativeScore neighbour queue
                            cameFrom' = M.insert neighbour (current, tag) cameFrom
                            gScore' = M.insert neighbour tentativeScore gScore
        traverseBack cameFrom current path
          | current == start = path
          | otherwise = traverseBack cameFrom prev (step : path)
          where step@(prev, _) = cameFrom M.! current
