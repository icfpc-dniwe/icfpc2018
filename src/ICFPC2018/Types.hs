module ICFPC2018.Types where

import Data.Vector (Vector)
import Data.IntSet (IntSet)
import ICFPC2018.Tensor3 (Tensor3)
import Linear.V3 (V3(..))

type Model = Tensor3 Bool

type BotIdx = Int
type BotSet = IntSet
type ShortDifference = V3 Int
type LongDifference = V3 Int
type NearDifference = V3 Int
data Command =
  -- Single commands
    Halt | Wait | Flip | SMove !LongDifference | LMove !ShortDifference !ShortDifference | Fission !NearDifference !Int | Fill !NearDifference
  -- Group commands
  | FusionP !NearDifference | FusionS !NearDifference
  deriving (Show, Eq)
type Step = Vector Command
type Trace = [Step]
