import Control.Monad
import System.Environment
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Binary.Put
import Linear.V3 (V3(..))

import ICFPC2018.IO
import ICFPC2018.Simulation
import ICFPC2018.Validation
import ICFPC2018.Solvers.HighSolver

main :: IO ()
main = do
  [modelPath, tracePath] <- getArgs
  modelData <- BL.readFile modelPath
  let model = runGet getModel modelData
      trace = packIntensions (solver model) (SingleBotModel (V3 0 0 0) model)
      traceData = runPut $ putTrace trace
  print trace
  unless (validTrace trace) $ fail "Invalid trace"
  BL.writeFile tracePath traceData
