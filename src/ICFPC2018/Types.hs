module ICFPC2018.Types where

import Data.Map (Map)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import ICFPC2018.Tensor3 (Tensor3, I3)
import Linear.V3 (V3(..))

type Model = Tensor3 Bool

-- each bot has a pool of seed indices
-- when performing a fission, it gives a subrange of indices to another bot
-- when perfroming a fusion, it takes back the bot id and its unused pool
type BotIdx = Int
type BotSeeds = IntSet
data Bot = Bot !BotIdx !BotSeeds

primaryBot :: Int {-N seeeds-} -> Bot
primaryBot n = Bot 0 $ IS.fromList $ [1..n]

type VolatileCoordinate = V3 Int
type Difference = V3 Int
type ShortDifference = Difference
type LongDifference  = Difference
type NearDifference  = Difference

data HarmonicState = Low | High deriving (Show, Eq)

changeHarmonic :: HarmonicState -> HarmonicState
changeHarmonic Low = High
changeHarmonic High = Low

data Command
  -- Single commands
  = Halt
  | Wait
  | Flip
  | SMove !LongDifference
  | LMove !ShortDifference !ShortDifference
  | Fission !NearDifference !Int
  | Fill !NearDifference
  -- Group commands
  | FusionP !NearDifference
  | FusionS !NearDifference
  deriving (Show, Eq)
type Step = Map BotIdx Command
type Trace = [Step]
type Score = Int

data Intension
  = FlipGravity
  | FillIdx I3
  deriving (Show, Eq)
type Intensions = [Intension]

data Axis = X | Y | Z deriving (Show, Eq, Enum, Bounded)

maxSLD :: Int
maxSLD = 5

maxLLD :: Int
maxLLD = 15
