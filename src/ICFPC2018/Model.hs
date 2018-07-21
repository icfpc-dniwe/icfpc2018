module ICFPC2018.Model
  ( showLayer
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

{-neighbours :: I3 -> Model -> [I3]
neighbours p model = filter (checkBounds (T3.size model)) $ map (p +) allNeighbours
  where allNeighbours =
          [ V3 (-1) 0    0
          , V3 1    0    0
          , V3 0    (-1) 0
          , V3 0    1    0
          , V3 0    0    (-1)
          , V3 0    0    1
          ]
-}

showLayer :: Int -> Model -> String
showLayer y model = intercalate "\n" $ map showLine [0..zS-1]
  where V3 xS _ zS = T3.size model
        showLine z = map (\x -> if model T3.! V3 x y z then '#' else ' ') [0..xS-1]

{-aStar :: I3 -> I3 -> Model -> Maybe [I3]
aStar start finish model = go (PQ.singleton (mlen start finish) start) (S.singleton start) S.empty M.empty (M.singleton start 0)
  where go queue0 openedSet0 closedSet0 cameFrom0 gScore0 =
          case PQ.getMin queue0 of
            Nothing -> Nothing
            Just (_, current)
              | current == finish -< Just $ traverseBack cameFrom0
              | otherwise -> foldr lookNeighbour (queue0, openedSet0, closedSet0, cameFrom0, gScore0) $ neighbours current model
              where currentScore = gScore M.! current
                    lookNeighbour (queue, openedSet, closedSet, cameFrom, gScore) neighbour = a
                      where tentativeScore = currentScore + mlen current neighbour
                            queue'
                              | neighbour `S.member` openedSet = queue
                              | otherwise = PQ.insert (mlen 
-}
