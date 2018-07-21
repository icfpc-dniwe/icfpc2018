module ICFPC2018.Solvers.ColumnSolver where

import ICFPC2018.Types
import qualified ICFPC2018.Tensor3 as T3

import Linear.V3 (V3(..))
import qualified Data.Vector as V

type BotCommands = [Command]

solve :: Model -> [BotCommands]
solve m = []