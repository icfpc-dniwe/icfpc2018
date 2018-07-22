module ICFPC2018.Tensor3
  ( Tensor3Size
  , T3View
  , I3
  , BoundingBox
  , Tensor3
  , T3
  , Axis(..)
  , index
  , (!)
  , size
  , update
  , create
  , replicate
  , boundingBox
  , slice
  , sliceAxis
  , inBounds
  , showY
  , showZ
  , scanY
  , scanZ
  ) where

import Prelude hiding (replicate)
import qualified Prelude as Pr
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
data T3 a = T3 !(Vector a) !Tensor3Size
          deriving (Show, Eq)
data T3View a = T3View
               { tensor :: !(T3 a)
               , closestIdx :: !I3
               , farthestIdx :: !I3
               , sizeView :: !Tensor3Size
               } deriving (Show, Eq)

instance Foldable T3View where
  foldMap fun view = foldMap (\idx -> fun $ (View view) ! idx) $ indexing $ size (View view)

instance Functor T3View where
  fmap f t = t { tensor = fmap f (tensor t)}

instance Traversable T3View where
  traverse f t = (\tensor' -> t { tensor = tensor'}) <$> (traverse f (tensor t))

data Tensor3 a = Tensor !(T3 a) | View !(T3View a) deriving (Show, Eq)

instance Foldable Tensor3 where
  foldMap f (Tensor t) = foldMap f t
  foldMap f (View v)   = foldMap f v

instance Functor Tensor3 where
  fmap f (Tensor t) = Tensor (fmap f t)
  fmap f (View v)   = View   (fmap f v)

instance Traversable Tensor3 where
  traverse f (Tensor t) = Tensor <$> (traverse f t)
  traverse f (View v)   = View   <$> (traverse f v)


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
  | inSizeBounds sz idx = linearIdx sz idx
  | otherwise = error "checkedLinearIdx: invalid index"

checkedIdx :: Tensor3Size -> I3 -> I3
checkedIdx sz idx
  | inSizeBounds sz idx = idx
  | otherwise = error "checkedIdx: invalid index"

size :: Tensor3 a -> Tensor3Size
size (Tensor tensor) = sizeT tensor
size (View (T3View {..})) = sizeView

sizeT :: T3 a -> Tensor3Size
sizeT (T3 _ sz) = sz

infixl 9 `index`
index :: Tensor3 a -> I3 -> a
index (Tensor (T3 v sz)) idx = v `V.unsafeIndex` checkedLinearIdx sz idx
index (View (T3View {..})) idx = (Tensor tensor) `index` (closestIdx + checkedIdx sizeView idx)

infixl 9 !
(!) :: Tensor3 a -> I3 -> a
(!) = index
{-
infixl 9 `indexView`
indexView :: Tensor3View a -> I3 -> a
indexView (Tensor3View {..}) idx = tensor ! (closestIdx + checkedIdx sizeView idx)

infixl 9 <!
(<!) :: Tensor3View a -> I3 -> a
(<!) = indexView
-}
update :: Tensor3 a -> [(I3, a)] -> Tensor3 a
update tensor [] = tensor
update (Tensor tensor) updates = Tensor $ tensor `updateT` updates
update (View tView) updates = View $ tView `updateView` updates

updateT :: T3 a -> [(I3, a)] -> T3 a
updateT tensor [] = tensor
updateT (T3 v sz) updates = T3 (V.unsafeUpd v $ map (first $ checkedLinearIdx sz) updates) sz

updateView :: T3View a -> [(I3, a)] -> T3View a
updateView tensor [] = tensor
updateView tensorView@(T3View {..}) updates = tensorView { tensor = tensor `updateT` map (first $ (closestIdx +) . (checkedIdx sizeView)) updates }

create :: Vector a -> Tensor3Size -> Tensor3 a
create v sz
  | V.length v == product sz = Tensor $ T3 v sz
  | otherwise = error "create: invalid tensor size"

createView :: T3 a -> BoundingBox -> T3View a
createView tensor@(T3 _ sz) (closestIdx, farthestIdx)
  | closestIdx >= (V3 0 0 0) &&
    closestIdx < farthestIdx &&
    farthestIdx < sz = let sizeView = farthestIdx - closestIdx in T3View {..}
  | otherwise = error "createView: invalid bounding box"

replicate :: Tensor3Size -> a -> Tensor3 a
replicate sz v = Tensor $ replicateT sz v

replicateT :: Tensor3Size -> a -> T3 a
replicateT sz v = T3 (V.replicate (product sz) v) sz

indexing :: Tensor3Size -> [I3]
indexing (V3 xSize ySize zSize) = [(V3 x y z) | x <- [0..xSize-1], y <- [0..ySize-1], z <- [0..zSize-1]]

boundingBox :: Tensor3 a -> (a -> Bool) -> BoundingBox
boundingBox tensor pr = foldr helper (sz - (V3 1 1 1), V3 0 0 0) (indexing sz)
  where
    sz = size tensor
    helper idx bbox@(closest, farthest)
      | pr (tensor ! idx) = (min <$> idx <*> closest, max <$> idx <*> farthest)
      | otherwise = bbox

slice :: Tensor3 a -> BoundingBox -> Tensor3 a
slice (Tensor tensor) bbox = View $ createView tensor bbox
slice (View (T3View {..})) (closestNew, farthestNew)
  | closestNew >= (V3 0 0 0) &&
    farthestNew < sizeView = View $ createView tensor bbox
  | otherwise = error "changeView: invalind bounding box"
    where
      bbox = (closestIdx + closestNew, closestIdx + farthestNew)

sliceAxis :: Tensor3 a -> Axis -> Int -> Int -> Tensor3 a
sliceAxis (Tensor tensor) axis = (View .) . (sliceAxisT tensor axis)
sliceAxis (View t3View) axis = (View .) . (sliceAxisView t3View axis)

sliceAxisT :: T3 a -> Axis -> Int -> Int -> T3View a
sliceAxisT tensor@(T3 _ (V3 _ ySize zSize)) X begin end = createView tensor (V3 begin 0 0, V3 end (ySize - 1) (zSize - 1))
sliceAxisT tensor@(T3 _ (V3 xSize _ zSize)) Y begin end = createView tensor (V3 0 begin 0, V3 (xSize - 1) end (zSize - 1))
sliceAxisT tensor@(T3 _ (V3 xSize ySize _)) Z begin end = createView tensor (V3 0 0 begin, V3 (xSize - 1) (ySize - 1) end)

sliceAxisView :: T3View a -> Axis -> Int -> Int -> T3View a
sliceAxisView (T3View {..}) X begin end = createView tensor (xBegin, xEnd)
  where
    xBegin = closestIdx + (V3 begin 0 0)
    xEnd = closestIdx + min farthestIdx (V3 end 0 0)
sliceAxisView (T3View {..}) Y begin end = createView tensor (yBegin, yEnd)
  where
    yBegin = closestIdx + (V3 0 begin 0)
    yEnd = closestIdx + min farthestIdx (V3 0 end 0)
sliceAxisView (T3View {..}) Z begin end = createView tensor (zBegin, zEnd)
  where
    zBegin = closestIdx + (V3 0 0 begin)
    zEnd = closestIdx + min farthestIdx (V3 0 0 end)

inBounds :: Tensor3 a -> I3 -> Bool
inBounds tensor = inSizeBounds (size tensor)

instance Foldable T3 where
  foldMap fun (T3 v _) = foldMap fun v

instance Functor T3 where
  fmap fun (T3 v sz) = T3 (fun <$> v) sz

instance Traversable T3 where
  traverse fun (T3 v sz) = T3 <$> traverse fun v <*> pure sz


showY :: (Show a) => Int -> Tensor3 a -> String
showY y t = unlines $ map (\z -> concatMap (\x -> show $ t ! (V3 x y z)) [0..(w-1)]) [0..(d-1)] where
  (V3 w _h d) = size t

showZ :: (Show a) => Int -> Tensor3 a -> String
showZ z t = unlines $ map (\y -> concatMap (\x -> show $ t ! (V3 x y z)) [0..(w-1)]) [0..(h-1)] where
  (V3 w h _d) = size t

scanY :: String -> Tensor3 Char
scanY s = (replicate (V3 n n n) ' ') `update` upd where
  ls = lines s
  n  = length ls
  enum = zip [0..(n-1)]
  upd = [(V3 x y z, v) | y <- [0..(n-1)], (z, w) <- (enum (map enum ls)), (x, v) <- w]

scanZ :: String -> Tensor3 Char
scanZ s = (replicate (V3 n n n) ' ') `update` upd where
  ls = lines s
  n  = length ls
  enum = zip [0..(n-1)]
  upd = [(V3 x y z, v) | z <- [0..(n-1)], (y, w) <- (enum (map enum ls)), (x, v) <- w]
