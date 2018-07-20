module ICFPC2018.Types where

import Data.Vector (Vector)
import ICFPC2018.Tensor3 (Tensor3)
import Linear.V3 (V3(..))
import qualified Linear.V3 as V3

type Model = Tensor3 Bool

type BotIdx = Int
type Distance = V3 Int
data SingleCommand = Halt | Wait | Flip | SMove Distance | LMove Distance | Fission Distance Int | Fill Int
                                   deriving (Show, Eq)
data GroupCommand = FusionP Distance | FusionS Distance
                                   deriving (Show, Eq)
data Command = SingleCommand | GroupCommand
type Trace = [(BotIdx, Command)]
