import Control.Monad
import Data.Maybe
import System.Environment
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Binary.Put
import Linear.V3 (V3(..))

import ICFPC2018.IO
import ICFPC2018.Pack
import ICFPC2018.Solvers.HighSolver
import ICFPC2018.Pipeline
import ICFPC2018.Simulation
import qualified ICFPC2018.Tensor3 as T3

import Debug.Trace

main :: IO ()
main = do
  [modelPath, tracePath] <- getArgs
  modelData <- BL.readFile modelPath
  let model = runGet getModel modelData
      V3 r _ _ = T3.size model
      state0 = initialState r
      solution = packSingleBotIntensions (stateMatrix state0) 1 0 $ solver model
      -- solution = pipeline model
      traceData = runPut $ putTrace solution
  unless (isJust $ foldM debugState (initialState r) solution) $ fail "Invalid trace"
  BL.writeFile tracePath traceData
