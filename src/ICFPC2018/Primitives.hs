module ICFPC2018.Primitives where

import ICFPC2018.Types
import ICFPC2018.Tensor3 (I3)
import qualified ICFPC2018.Tensor3 as T3
import Linear.V3 (V3(..))
import Data.Map as M

type EnterPos = I3
type ExitPos = I3

data Primitive = Primitive
                 { enterPos :: ![EnterPos]
                 , exitPos :: ![EnterPos -> ExitPos]
                 , numBots :: !Int
                 , commands :: EnterPos -> Trace
                 , filled :: Model
                 , primitiveSize :: EnterPos -> Model
                 }

voxel :: Primitive
voxel = Primitive
        { enterPos = [(V3 x y z) | x <- [0..2], y <- [0..2], z <- [0..2], not (x == y && y == z)]
        , exitPos = [id]
        , numBots = 1
        , commands = \pos -> [M.singleton 0 (Fill $ center - pos)]
        , filled = nullTensor `T3.update` [(center, True)]
        , primitiveSize = \pos -> nullTensor `T3.update` [(center, True), (pos, True)]
        }
          where
            center = V3 1 1 1
            nullTensor = T3.replicate (V3 3 3 3) False

x2Line :: Primitive
x2Line = Primitive
         { enterPos = [(V3 x y z) | x <- [0, 1], y <- [0..2], z <- [0..2], not (y == 1 && z == 1)]
         , exitPos = [id]
         , numBots = 1
         , commands = \pos -> [M.fromList [(0, (Fill $ centerYZ 0 - pos)), (0, (Fill $ centerYZ 1 - pos))]]
         , filled = filledTensor
         , primitiveSize = \pos -> filledTensor `T3.update` [(pos, True)]
         }
           where
             nullTensor = T3.replicate (V3 2 3 3) False
             filledTensor = nullTensor `T3.update` [(centerYZ 0, True), (centerYZ 1, True)]
             centerYZ x = V3 x 1 1
