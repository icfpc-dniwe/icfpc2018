module ICFPC2018.Types where

import qualified Data.Vector as V
import qualified Data.Linear.V3 as V3

data Tensor3 a = Tensor3 (V a) V3
