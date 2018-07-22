module ICFPC2018.Tensor3
  ( Tensor3Size
  , Tensor3View
  , I3
  , BoundingBox
  , Tensor3
  , index
  , (!)
  , indexView
  , (<!)
  , size
  , sizeView
  , update
  , updateView
  , create
  , createView
  , replicate
  , boundingBox
  , subView
  , slice
  , sliceView
  , sliceAxis
  , sliceAxisView
  , inBounds
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

data Axis = X | Y | Z deriving (Show, Eq, Enum, Bounded)
data Tensor3 a = Tensor3 !(Vector a) !Tensor3Size
               deriving (Show, Eq)
data Tensor3View a = Tensor3View
                     { tensor :: !(Tensor3 a)
                     , closestIdx :: !I3
                     , farthestIdx :: !I3
                     , sizeView :: !Tensor3Size
                     } deriving (Show, Eq)

linearIdx :: Tensor3Size -> I3 -> Int
linearIdx (V3 _ ySize zSize) (V3 xIdx yIdx zIdx) = zIdx + yIdx * zSize + xIdx * zSize * ySize

{-
tensorIdx :: Int -> Tensor3Size -> I3
tensorIdx linIdx (V3 xSize ySize zSize) = (V3 x y z) where
  (x, xrest) = (linIdx `div` (zSize * ySize), linIdx `mod` (zSize * ySize))
  (y, z) = (xrest `div` zSize, xrest `mod` zSize)
-}

checkedLinearIdx :: Tensor3Size -> I3 -> Int
checkedLinearIdx sz idx
  | inBox sz idx = linearIdx sz idx
  | otherwise = error "checkedLinearIdx: invalid index"

checkedIdx :: Tensor3Size -> I3 -> I3
checkedIdx sz idx
  | checkBounds sz idx = idx
  | otherwise = error "checkedIdx: invalid index"

size :: Tensor3 a -> Tensor3Size
size (Tensor3 _ sz) = sz

infixl 9 `index`
index :: Tensor3 a -> I3 -> a
index (Tensor3 v sz) idx = v `V.unsafeIndex` checkedLinearIdx sz idx

infixl 9 !
(!) :: Tensor3 a -> I3 -> a
(!) = index

infixl 9 `indexView`
indexView :: Tensor3View a -> I3 -> a
indexView (Tensor3View {..}) idx = tensor ! (closestIdx + checkedIdx sizeView idx)

infixl 9 <!
(<!) :: Tensor3View a -> I3 -> a
(<!) = indexView

update :: Tensor3 a -> [(I3, a)] -> Tensor3 a
update tensor [] = tensor
update (Tensor3 v sz) updates = Tensor3 (V.unsafeUpd v $ map (first $ checkedLinearIdx sz) updates) sz

updateView :: Tensor3View a -> [(I3, a)] -> Tensor3View a
updateView tensorView@(Tensor3View {..}) updates = tensorView { tensor = tensor `update` map (first $ (closestIdx +) . (checkedIdx sizeView)) updates }

create :: Vector a -> Tensor3Size -> Tensor3 a
create v sz
  | V.length v == product sz = Tensor3 v sz
  | otherwise = error "create: invalid tensor size"

createView :: Tensor3 a -> BoundingBox -> Tensor3View a
createView tensor@(Tensor3 _ sz) (closestIdx, farthestIdx)
  | closestIdx >= (V3 0 0 0) &&
    closestIdx < farthestIdx &&
    farthestIdx < sz = let sizeView = farthestIdx - closestIdx in Tensor3View {..}
  | otherwise = error "createView: invalid bounding box"

replicate :: Tensor3Size -> a -> Tensor3 a
replicate sz v = Tensor3 (V.replicate (product sz) v) sz

indexing :: Tensor3Size -> [I3]
indexing (V3 xSize ySize zSize) = [(V3 x y z) | x <- [0..xSize-1], y <- [0..ySize-1], z <- [0..zSize-1]]

boundingBox :: Tensor3 a -> (a -> Bool) -> BoundingBox
boundingBox tensor pr = foldr helper (sz - (V3 1 1 1), V3 0 0 0) (indexing sz)
  where
    sz = size tensor
    helper idx bbox@(closest, farthest)
      | pr (tensor ! idx) = (min <$> idx <*> closest, max <$> idx <*> farthest)
      | otherwise = bbox

subView :: Tensor3View a -> (a -> Bool) -> Tensor3View a
subView tensorView@(Tensor3View {..}) pr = sliceView tensorView $ boundingBox tensor pr

-- TODO: change to Tensor3 a -> BoundingBox -> Tensor3View
slice :: Tensor3 a -> BoundingBox -> [I3]
slice tensor ((V3 x0 y0 z0), (V3 x1 y1 z1)) = filter (\(V3 x y z) -> all id [x0 <= x, x1 >= x, y0 <= y, y1 >= y, z0 <= z, z1 >= z]) $ indexing $ size tensor

sliceView :: Tensor3View a -> BoundingBox -> Tensor3View a
sliceView (Tensor3View {..}) (closestNew, farthestNew)
  | closestNew >= (V3 0 0 0) &&
    farthestNew < sizeView = createView tensor bbox
  | otherwise = error "changeView: invalind bounding box"
    where
      bbox = (closestIdx + closestNew, closestIdx + farthestNew)

sliceAxis :: Tensor3 a -> Axis -> Int -> Int -> Tensor3View a
sliceAxis tensor@(Tensor3 _ (V3 _ ySize zSize)) X begin end = createView tensor (V3 begin 0 0, V3 end (ySize - 1) (zSize - 1))
sliceAxis tensor@(Tensor3 _ (V3 xSize _ zSize)) Y begin end = createView tensor (V3 0 begin 0, V3 (xSize - 1) end (zSize - 1))
sliceAxis tensor@(Tensor3 _ (V3 xSize ySize _)) Z begin end = createView tensor (V3 0 0 begin, V3 (xSize - 1) (ySize - 1) end)

sliceAxisView :: Tensor3View a -> Axis -> Int -> Int -> Tensor3View a
sliceAxisView (Tensor3View {..}) X begin end = createView tensor (xBegin, xEnd)
  where
    xBegin = closestIdx + (V3 begin 0 0)
    xEnd = closestIdx + min farthestIdx (V3 end 0 0)
sliceAxisView (Tensor3View {..}) Y begin end = createView tensor (yBegin, yEnd)
  where
    yBegin = closestIdx + (V3 0 begin 0)
    yEnd = closestIdx + min farthestIdx (V3 0 end 0)
sliceAxisView (Tensor3View {..}) Z begin end = createView tensor (zBegin, zEnd)
  where
    zBegin = closestIdx + (V3 0 0 begin)
    zEnd = closestIdx + min farthestIdx (V3 0 0 end)

inBounds :: Tensor3 a -> I3 -> Bool
inBounds tensor = inBox (size tensor)

instance Foldable Tensor3 where
  foldMap fun (Tensor3 v _) = foldMap fun v

instance Functor Tensor3 where
  fmap fun (Tensor3 v sz) = Tensor3 (fun <$> v) sz

instance Traversable Tensor3 where
  traverse fun (Tensor3 v sz) = Tensor3 <$> traverse fun v <*> pure sz
