module ICFPC2018.Types where

import Data.IntSet (IntSet)
import ICFPC2018.Tensor3 (Tensor3)
import Linear.V3 (V3(..))

type Model = Tensor3 Bool

type BotIdx = Int
type BotSet = IntSet
type Distance = V3 Int
data SingleCommand = Halt | Wait | Flip | SMove !Distance | LMove !Distance !Distance | Fission !Distance !Int | Fill !Int
                   deriving (Show, Eq)
data GroupCommand = FusionP !Distance | FusionS !Distance
                  deriving (Show, Eq)
data Command = SCommand !BotIdx !SingleCommand | GCommand !BotSet !GroupCommand
             deriving (Show, Eq)
type Trace = [Command]
