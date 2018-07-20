module ICFPC2018.Solvers.HighSolver
  (solver) where

import ICFPC2018.Types
import qualified ICFPC2018.Tensor3 as T3
import ICFPC2018.Utils (snakeIdx)

solver :: Model -> Intensions
solver model = FlipGravity : map (\v -> FillIdx v) (filter (\idx -> model T3.! idx) (snakeIdx $ T3.size model)) ++ [FlipGravity]
