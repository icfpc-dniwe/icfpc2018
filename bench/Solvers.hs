import Control.Monad
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Linear.V3 (V3(..))
import Criterion.Main

import ICFPC2018.Types
import ICFPC2018.IO
import ICFPC2018.Pack
import ICFPC2018.Pipeline
import ICFPC2018.Simulation
import ICFPC2018.Solvers.HighSolver
import qualified ICFPC2018.Tensor3 as T3

setupEnv :: IO (Model, ExecState, Trace)
setupEnv = do
  modelData <- BL.readFile "tasks/lightning/LA100_tgt.mdl"
  let model = runGet getModel modelData
      V3 r _ _ = T3.size model
      state0 = initialState r
      solution = packSingleBotIntensions (stateMatrix state0) 1 0 $ solver model
  return (model, state0, solution)

main :: IO ()
main = defaultMain
  [ env setupEnv $ \ ~(model, state0, solution) -> bgroup "Solvers"
    [ bench "High" $ nf (packSingleBotIntensions (stateMatrix state0) 1 0 . solver) model
    , bench "Pipeline" $ nf pipeline model
    , bench "Simulation" $ nf (foldM stepState state0) solution
    ]
  ]
