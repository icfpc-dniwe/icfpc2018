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

data Tensor3 a = Tensor3 !(Vector a) !Tensor3Size
               deriving (Show, Eq)

linearIdx :: Tensor3Size -> I3 -> Int
linearIdx (V3 xSize ySize zSize) (V3 xIdx yIdx zIdx) = zIdx + yIdx * zSize + xIdx * zSize * ySize

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

instance Foldable Tensor3 where
  foldMap fun (Tensor3 v sz) = foldMap fun v

instance Functor Tensor3 where
  fmap fun (Tensor3 v sz) = Tensor3 (fun <$> v) sz

instance Traversable Tensor3 where
  traverse fun (Tensor3 v sz) = Tensor3 <$> traverse fun v <*> pure sz
