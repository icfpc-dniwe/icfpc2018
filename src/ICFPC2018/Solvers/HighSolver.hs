module ICFPC2018.Solvers.HighSolver
  (solver) where

import ICFPC2018.Types
import qualified ICFPC2018.Tensor3 as T3

solver :: Model -> Intensions
solver model = FlipGravity : map (\v -> FillIdx v) (T3.snakeIdx model) ++ [FlipGravity]
