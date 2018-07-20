module ICFPC2018.Tensor3
  ( Tensor3Size
  , Tensor3Idx
  , Tensor3
  , index
  , (!)
  , size
  , update
  , create
  , snakeIdx
  ) where

import Control.Arrow
import Data.Vector (Vector)
import qualified Data.Vector as V
import Linear.V3 (V3(..))

type Tensor3Size = V3 Int
type Tensor3Idx = V3 Int

data Tensor3 a = Tensor3 !(Vector a) !Tensor3Size
               deriving (Show, Eq)

linearIdx :: Tensor3Size -> Tensor3Idx -> Int
linearIdx (V3 xSize ySize _) (V3 xIdx yIdx zIdx) = xIdx + yIdx * xSize + zIdx * xSize * ySize

checkedLinearIdx :: Tensor3Size -> Tensor3Idx -> Int
checkedLinearIdx sz@(V3 xSize ySize zSize) idx@(V3 xIdx yIdx zIdx)
  | xIdx < xSize && xIdx >= 0 &&
    yIdx < ySize && yIdx >= 0 &&
    zIdx < zSize && zIdx >= 0
  = linearIdx sz idx
  | otherwise = error "checkedLinearIdx: invalid index"

tensorIdx :: Tensor3Size -> Int -> Tensor3Idx
tensorIdx (V3 xSize ySize _) idx = V3 xIdx yIdx zIdx
  where (zIdx, surfIdx) = idx `divMod` (xSize * ySize)
        (yIdx, xIdx) = surfIdx `divMod` ySize

size :: Tensor3 a -> Tensor3Size
size (Tensor3 _ sz) = sz

index :: Tensor3 a -> Tensor3Idx -> a
index (Tensor3 v sz) idx = v `V.unsafeIndex` checkedLinearIdx sz idx

infixl 9 !
(!) :: Tensor3 a -> Tensor3Idx -> a
(!) = index

update :: Tensor3 a -> [(Tensor3Idx, a)] -> Tensor3 a
update tensor [] = tensor
update (Tensor3 v sz) updates = Tensor3 (V.unsafeUpd v $ map (first $ checkedLinearIdx sz) updates) sz

create :: Vector a -> Tensor3Size -> Tensor3 a
create v sz
  | V.length v == product sz = Tensor3 v sz
  | otherwise = error "create: invalid tensor size"

{-
  moving across Tensor3 starting from (0, 0, 0) along X first then Z and finally Y.
  Y == 0 is bottom plane.
-}
snakeIdx :: Tensor3 a -> [Tensor3Idx]
snakeIdx (Tensor3 _ (V3 xSize ySize zSize)) = helpFold (V3 0 0 0) False
  where
    helpFold (V3 xIdx yIdx zIdx) ySwitch
      | yIdx >= ySize = []
      | firstX = map xToV3 [0 .. xSize - 1] ++ changeZ (xSize - 1)
      | lastX = map xToV3 [xSize - 1, xSize - 2 .. 0] ++ changeZ 0
      | otherwise = undefined
      where
        firstX = xIdx == 0
        firstY = yIdx == 0
        firstZ = zIdx == 0
        lastX = xIdx == xSize - 1
        lastY = yIdx == ySize - 1
        lastZ = zIdx == zSize - 1
        xToV3 x = (V3 x yIdx zIdx)
        changeZ x | lastZ && not ySwitch = changeY x
                  | firstZ && ySwitch    = changeY x
                  | not ySwitch = helpFold (V3 x yIdx (zIdx + 1)) ySwitch
                  | ySwitch = helpFold (V3 x yIdx (zIdx - 1)) ySwitch
        changeY x = helpFold (V3 x (yIdx + 1) zIdx) (not ySwitch)
