module ICFPC2018.Solvers.HighSolver
  (solver) where

import ICFPC2018.Types
import qualified ICFPC2018.Tensor3 as T3

solver :: Model -> Trace
solver model = undefined

addFlip :: Command
addFlip = Flip

moveToZero :: T3.Tensor3Idx -> Trace
moveToZero = undefined
