module ICFPC2018.Tensor3
  ( Tensor3Size
  , Tensor3Idx
  , Tensor3
  , index
  , update
  , generate
  ) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Linear.V3 (V3)
import qualified Linear.V3 as V3

type Tensor3Size = V3 Int
type Tensor3Idx = V3 Int

data Tensor3 a = Tensor3 (Vector a) Tensor3Size

resultIdx :: Tensor3Size -> Tensor3Idx -> Int
resultIdx (V3.V3 xSize ySize zSize) (V3.V3 xIdx yIdx zIdx)
  | xIdx < xSize && yIdx < ySize && zIdx < zSize && xIdx >= 0 && yIdx >= 0  && zIdx >= 0 = xIdx + yIdx * xSize + zIdx * xSize * ySize
  | otherwise = error "Invalid index"

index :: Tensor3 a -> Tensor3Idx -> a
index (Tensor3 v size) idx = v `V.unsafeIndex` resultIdx size idx

update :: Tensor3 a -> [(Tensor3Idx, a)] -> Tensor3 a
update tensor [] = tensor
update (Tensor3 v size) updates = Tensor3 (V.unsafeUpd v $ map (\(idx, val) -> (resultIdx size idx, val)) updates) size

generate :: Vector a -> Tensor3Size -> Tensor3 a
generate v size@(V3.V3 xSize ySize zSize)
  | V.length v == xSize * ySize * zSize = Tensor3 v size
  | otherwise = error "invalid tensor size"
