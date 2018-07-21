module ICFPC2018.Solvers.ColumnSolver where

import ICFPC2018.Types
import qualified ICFPC2018.Tensor3 as T3

import Linear.V3 (V3(..))
import qualified Data.Vector as V

{-
type NBot = Int
type LayerID = Int
type BoundingBox = (I3, I3)

generateBots = Model -> NBot -> BoundingBox -> []

fillLayer :: Model -> NBot -> BoundingBox -> LayerID -> Trace
fillLayer model nbot bbox y = 

solve :: Model -> NBot -> Trace
solve model nbot =
    if needBots <= nbot
        then map (++) $ fillLayer model needBots bbox <$> [by0..by1]
        else error "not implemented"
    where
        bbox@((V3 bx0 by0 bz0), (V3 bx1 by1 bz1)) = T3.boundingBox model id
        bdx = abs $ bx1 - bx0
        bdz = abs $ bz1 - bz0
        needBots = bdx * bdz `div` 9
-}