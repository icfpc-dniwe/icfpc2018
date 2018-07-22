import Control.Monad
import Data.Maybe
import System.Environment
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Binary.Put
import Linear.V3 (V3(..))

import ICFPC2018.IO
import ICFPC2018.Pack
import ICFPC2018.Validation
import ICFPC2018.Solvers.HighSolver
import ICFPC2018.Pipeline
import ICFPC2018.Simulation
import ICFPC2018.Pack
import qualified ICFPC2018.Tensor3 as T3

main :: IO ()
main = do
  [modelPath, tracePath] <- getArgs
  modelData <- BL.readFile modelPath
  let model = runGet getModel modelData
      V3 r _ _ = T3.size model
      trace = packSingleBotIntensions model 1 0 $ solver model
      -- trace = pipeline model
      traceData = runPut $ putTrace trace
  unless (isJust $ foldM stepState (initialState r) trace) $ fail "Invalid trace"
  BL.writeFile tracePath traceData
