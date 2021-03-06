module ICFPC2018.Types where

import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import ICFPC2018.Tensor3 (MTensor3, Tensor3, I3, Axis(..))
import Linear.V3 (V3(..))
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

type Model = Tensor3 Bool
type MModel s = MTensor3 s Bool

-- each bot has a pool of seed indices
-- when performing a fission, it gives a subrange of indices to another bot
-- when perfroming a fusion, it takes back the bot id and its unused pool
type BotIdx = Int
type BotSeeds = IntSet
type BotPos = V3 Int
data Bot = Bot !BotIdx !BotPos !BotSeeds
    deriving (Show, Eq)

primaryBot :: BotPos -> Int {-N seeeds-} -> Bot
primaryBot p n = Bot 0 p $ IS.fromList [1..n]

type VolatileCoordinate = V3 Int
type Difference = V3 Int
type ShortDifference = Difference
type LongDifference  = Difference
type NearDifference  = Difference
type FarDifference   = Difference

data HarmonicState = Low | High deriving (Show, Eq, Generic)

instance NFData HarmonicState

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
  | Void !NearDifference
  -- Group commands
  | FusionP !NearDifference
  | FusionS !NearDifference
  | GFill !NearDifference !FarDifference
  | GVoid !NearDifference !FarDifference
  deriving (Show, Eq, Generic)

instance NFData Command

isFission :: Command -> Bool
isFission (Fission _ _) = True
isFission _             = False

isFusionP :: Command -> Bool
isFusionP (FusionP _) = True
isFusionP _             = False

isFusionS :: Command -> Bool
isFusionS (FusionS _) = True
isFusionS _             = False

type Step = IntMap Command
type Trace = [Step]
type Score = Int

data Intension
  = FlipGravity
  | FillIdx I3
  deriving (Show, Eq)
type Intensions = [Intension]

-- data Axis = X | Y | Z deriving (Show, Eq, Enum, Bounded)

maxSLD :: Int
maxSLD = 5

maxLLD :: Int
maxLLD = 15

maxFD :: Int
maxFD = 30

mkLinearDifference :: Axis -> Int -> Difference
mkLinearDifference X v = V3 v 0 0
mkLinearDifference Y v = V3 0 v 0
mkLinearDifference Z v = V3 0 0 v
