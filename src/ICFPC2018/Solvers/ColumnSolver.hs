module ICFPC2018.Solvers.ColumnSolver where

import ICFPC2018.Types
import ICFPC2018.Tensor3 (I3)
import qualified ICFPC2018.Tensor3 as T3

import Linear.V3 (V3(..))
import qualified Data.Vector as V

--solve :: Model -> Int {-N bots-} -> Trace
--solve model = [] where
--    ((I3 bx0 by0 bz0), (I3 bx1 by1 bz1)) = T3.boundingBox model id