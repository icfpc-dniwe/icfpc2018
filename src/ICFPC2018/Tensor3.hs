module ICFPC2018.Tensor3
  ( Tensor3Size
  , Tensor3Idx
  , Tensor3
  , index
  , (!)
  , size
  , update
  , create
  ) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Linear.V3 (V3(..))

type Tensor3Size = V3 Int
type Tensor3Idx = V3 Int

data Tensor3 a = Tensor3 !(Vector a) !Tensor3Size
               deriving (Show, Eq)

resultIdx :: Tensor3Size -> Tensor3Idx -> Int
resultIdx (V3 xSize ySize zSize) (V3 xIdx yIdx zIdx)
  | xIdx < xSize && xIdx >= 0 &&
    yIdx < ySize && yIdx >= 0 &&
    zIdx < zSize && zIdx >= 0
      = xIdx + yIdx * xSize + zIdx * xSize * ySize
  | otherwise = error "Invalid index"

size :: Tensor3 a -> Tensor3Size
size (Tensor3 _ sz) = sz

index :: Tensor3 a -> Tensor3Idx -> a
index (Tensor3 v sz) idx = v `V.unsafeIndex` resultIdx sz idx

infixl 9 !
(!) :: Tensor3 a -> Tensor3Idx -> a
(!) = index

update :: Tensor3 a -> [(Tensor3Idx, a)] -> Tensor3 a
update tensor [] = tensor
update (Tensor3 v sz) updates = Tensor3 (V.unsafeUpd v $ map (\(idx, val) -> (resultIdx sz idx, val)) updates) sz

create :: Vector a -> Tensor3Size -> Tensor3 a
create v sz
  | V.length v == product sz = Tensor3 v sz
  | otherwise = error "invalid tensor size"
