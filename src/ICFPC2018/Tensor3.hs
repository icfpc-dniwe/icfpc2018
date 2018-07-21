module ICFPC2018.Tensor3
  ( Tensor3Size
  , I3
  , Tensor3
  , index
  , (!)
  , size
  , update
  , create
  , replicate
  ) where

import Prelude hiding (replicate)
import Control.Arrow
import Data.Vector (Vector)
import qualified Data.Vector as V
import Linear.V3 (V3(..))

import ICFPC2018.Utils

type Tensor3Size = V3 Int
type I3 = V3 Int

-- first index in the closest (x y z), second index is included in bounding box
type BoundingBox = (I3, I3)

data Tensor3 a = Tensor3 !(Vector a) !Tensor3Size
               deriving (Show, Eq)

linearIdx :: Tensor3Size -> I3 -> Int
linearIdx (V3 xSize ySize zSize) (V3 xIdx yIdx zIdx) = zIdx + yIdx * zSize + xIdx * zSize * ySize

tensorIdx :: Int -> Tensor3Size -> I3
tensorIdx linIdx (V3 xSize ySize zSize) = (V3 x y z) where
  (x, xrest) = (linIdx `div` (zSize * ySize), linIdx `mod` (zSize * ySize))
  (y, z) = (xrest `div` zSize, xrest `mod` zSize)

checkedLinearIdx :: Tensor3Size -> I3 -> Int
checkedLinearIdx sz idx
  | checkBounds sz idx = linearIdx sz idx
  | otherwise = error "checkedLinearIdx: invalid index"

size :: Tensor3 a -> Tensor3Size
size (Tensor3 _ sz) = sz

infixl 9 `index`
index :: Tensor3 a -> I3 -> a
index (Tensor3 v sz) idx = v `V.unsafeIndex` checkedLinearIdx sz idx

infixl 9 !
(!) :: Tensor3 a -> I3 -> a
(!) = index

update :: Tensor3 a -> [(I3, a)] -> Tensor3 a
update tensor [] = tensor
update (Tensor3 v sz) updates = Tensor3 (V.unsafeUpd v $ map (first $ checkedLinearIdx sz) updates) sz

create :: Vector a -> Tensor3Size -> Tensor3 a
create v sz
  | V.length v == product sz = Tensor3 v sz
  | otherwise = error "create: invalid tensor size"

replicate :: Tensor3Size -> a -> Tensor3 a
replicate sz v = Tensor3 (V.replicate (product sz) v) sz

indexing :: Tensor3Size -> [I3]
indexing (V3 xSize ySize zSize) = [(V3 x y z) | x <- [0..xSize-1], y <- [0..ySize-1], z <- [0..zSize-1]]

boundingBox :: Tensor3 a -> (a -> Bool) -> BoundingBox
boundingBox tensor@(Tensor3 v sz) pr = foldr helper (sz - (V3 1 1 1), V3 0 0 0) (indexing sz)
  where
    helper idx bbox@(closest, farthest)
      | pr (tensor ! idx) = (min <$> idx <*> closest, max <$> idx <*> farthest)
      | otherwise = bbox

instance Foldable Tensor3 where
  foldMap fun (Tensor3 v sz) = foldMap fun v

instance Functor Tensor3 where
  fmap fun (Tensor3 v sz) = Tensor3 (fun <$> v) sz

instance Traversable Tensor3 where
  traverse fun (Tensor3 v sz) = Tensor3 <$> traverse fun v <*> pure sz
