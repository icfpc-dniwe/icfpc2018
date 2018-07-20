module ICFPC2018.Types where

import Data.Vector (Vector)
import ICFPC2018.Tensor3 (Tensor3)
import Linear.V3 (V3(..))
import qualified Linear.V3 as V3

type Model = Tensor3 Bool

type BotIdx = Int
type Distance = V3 Int
data Command = Halt | Wait | Flip | SMove Distance | LMove Distance | Fission Distance Int | Fill Int
                              deriving (Show, Eq)
type Trace = [(BotIdx, Command)]
