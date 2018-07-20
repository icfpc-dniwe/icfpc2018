module ICFPC2018.Tensor3 where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Linear.V3 (V3)
import qualified Data.Linear.V3 as V3

data Tensor3 a = Tensor3 (Vector a) V3
