module ICFPC2018.Model
  ( showLayer
  , aStar
  --, VoxelStatus(..)
  --, decomposeModel
  --, floodFill
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

{-# INLINE aStar #-}
aStar :: forall i tag metric. (Show i, Show metric, Ord i, Ord metric, Num metric) => (i -> [(i, tag)]) -> (i -> i -> metric) -> i -> i -> Maybe [(i, tag)]
aStar getNeighbours metric start finish = go (PQ.singleton (metric start finish) start) S.empty M.empty (M.singleton start 0)
  where go :: MinPQueue metric i -> Set i -> Map i (i, tag) -> Map i metric -> Maybe [(i, tag)]
        -- gScore -- cost of getting from start node to that node
        -- queue weight -- cost of getting from start node to goal by passing that node
        go queue0 closedSet0 cameFrom0 gScore0 =
          case PQ.minView queue0 of
            Nothing -> Nothing
            Just (current, queue0')
              | current == finish -> Just $ traverseBack cameFrom0 finish []
              | current `S.member` closedSet0 -> go queue0' closedSet0 cameFrom0 gScore0
              | otherwise -> go queue1 (S.insert current closedSet0) cameFrom1 gScore1
              where currentScore = gScore0 M.! current
                    (queue1, cameFrom1, gScore1) = lookNeighbours queue0' cameFrom0 gScore0 $ getNeighbours current

                    lookNeighbours :: MinPQueue metric i -> Map i (i, tag) -> Map i metric -> [(i, tag)] -> (MinPQueue metric i, Map i (i, tag), Map i metric)
                    lookNeighbours queue cameFrom gScore [] = (queue, cameFrom, gScore)
                    lookNeighbours queue cameFrom gScore ((neighbour, tag):neighbours) =
                      case M.lookup neighbour gScore of
                        Just score | score <= tentativeScore -> lookNeighbours queue cameFrom gScore neighbours
                        -- Early stop if finish is found.
                        _ | neighbour == finish -> (queue', cameFrom', gScore')
                          | otherwise -> lookNeighbours queue' cameFrom' gScore' neighbours
                      where tentativeScore = currentScore + metric current neighbour
                            queue' = PQ.insert (tentativeScore + metric neighbour finish) neighbour queue
                            cameFrom' = M.insert neighbour (current, tag) cameFrom
                            gScore' = M.insert neighbour tentativeScore gScore
        traverseBack cameFrom current path
          | current == start = path
          | otherwise = traverseBack cameFrom prev (step `seq` (step : path))
          where step@(prev, _) = cameFrom M.! current

{-
data VoxelStatus
  = VSUnknown
  | VSEmpty
  | VSGrounded
  | VSFloating
  deriving Eq

instance Show VoxelStatus where
  show VSUnknown  = "@"
  show VSEmpty    = " "
  show VSGrounded = "."
  show VSFloating = "!"

decomposeModel :: Model -> ([(I3, VoxelStatus)], Tensor3 VoxelStatus)
decomposeModel m
  = second (fmap (fromRight VSUnknown))
  . first catMaybes
  $ runState (mapM step idxs) (fmap Left m)
  where
    V3 w h d = T3.size m
    idxs = [(V3 x y z) | y <- [0..(h-1)], x <- [0..(w-1)], z <- [0..(d-1)]]

    step :: I3 -> State (Tensor3 (Either Bool VoxelStatus)) (Maybe (I3, VoxelStatus))
    step idx@(V3 _ y _) = get >>= \s -> case (s T3.! idx) of
      Left b -> do
        let t = case b of
                  False -> VSEmpty
                  True  -> if (y == 0) then VSGrounded else VSFloating
        put (floodFill idx (Right t) s)
        return $ Just (idx, t)
      _ -> return Nothing


floodFill :: forall a. (Eq a) => I3 -> a -> Tensor3 a -> Tensor3 a
floodFill idx0 v m0 = execState (step idx0) m0 where
  sz = T3.size m0

  step :: I3 -> State (Tensor3 a) ()
  step idx = gets (T3.! idx) >>= \v0 -> case (v0 == v) of
    True  -> return ()
    False -> do
      modify (\m -> T3.update m [(idx, v)])
      mapM_ (\idx' -> get >>= \m' -> when (m' T3.! idx' == v0) $ step idx') (adjacent idx)

  adjacent :: I3 -> [I3]
  adjacent idx = filter (inSizeBounds sz) . map (idx+) $ [
      (V3 1 0 0), -(V3 1 0 0)
    , (V3 0 1 0), -(V3 0 1 0)
    , (V3 0 0 1), -(V3 0 0 1)
    ]
-}
