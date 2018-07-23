module ICFPC2018.Tensor3
  ( Tensor3Size
  , I3
  , BoundingBox
  , Tensor3
  , MTensor3
  , Axis(..)
  , index
  , (!)
  , copyView
  , size
  , axisSize
  , update
  , fill
  , fillBox
  , create
  , replicate
  , boundingBox
  , slice
  , sliceAxis
  , inBounds
  , nonzero
  , showY
  , showZ
  , scanY
  , scanZ
  , freeze
  , thaw
  , unsafeFreeze
  , unsafeThaw
  , read
  , write
  , mSize
  ) where

import Prelude hiding (read, replicate)
import Control.Arrow
import Data.Vector.Unboxed (Vector, Unbox)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Generic as MV
import qualified Data.Vector.Unboxed.Mutable as MV
import Linear.V3 (V3(..))
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Control.Monad.Primitive (PrimMonad(..))

import ICFPC2018.Utils

type Tensor3Size = V3 Int
type I3 = V3 Int

-- first index in the closest (x y z), second index is included in bounding box
type BoundingBox = (I3, I3)

data Axis = X | Y | Z deriving (Show, Eq, Enum, Bounded)

data T3 a = T3 !(Vector a) !Tensor3Size
          deriving (Show, Eq, Generic)

data T3View a = T3View
                { tensor :: !(T3 a)
                , closestIdx :: !I3
                , farthestIdx :: !I3
                , sizeView :: !Tensor3Size
                } deriving (Show, Eq, Generic)

data Tensor3 a = Tensor !(T3 a) | View !(T3View a) deriving (Show, Generic)

data MTensor3 s a = MT3 !(MVector s a) !(Tensor3Size)

instance NFData a => NFData (T3 a)

instance NFData a => NFData (T3View a)

instance NFData a => NFData (Tensor3 a)

instance (Unbox a, Eq a) => Eq (Tensor3 a) where
  (Tensor t1) == (Tensor t2) = t1 == t2
  t1 == t2 | size t1 == size t2 = foldr (\idx res -> res && (t1 ! idx == t2 ! idx)) False $ indexing (size t1)
           | otherwise = False

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
index :: Unbox a => Tensor3 a -> I3 -> a
index (Tensor (T3 v sz)) idx = lidx `seq` (v `V.unsafeIndex` lidx)
  where lidx = checkedLinearIdx sz idx
index (View (T3View { tensor = T3 dat sz, .. })) idx = lidx `seq` (dat `V.unsafeIndex` lidx)
  where lidx = linearIdx sz (closestIdx + checkedIdx sizeView idx)

infixl 9 !
(!) :: Unbox a => Tensor3 a -> I3 -> a
(!) = index

copyView :: Unbox a => T3View a -> T3 a
copyView (T3View { tensor = T3 dat sz, .. }) = T3 (V.fromList $ map (V.unsafeIndex dat . linearIdx sz) $ boxIndices closestIdx farthestIdx) sizeView

{-
infixl 9 `indexView`
indexView :: Tensor3View a -> I3 -> a
indexView (Tensor3View {..}) idx = tensor ! (closestIdx + checkedIdx sizeView idx)

infixl 9 <!
(<!) :: Tensor3View a -> I3 -> a
(<!) = indexView
-}
update :: Unbox a => Tensor3 a -> [(I3, a)] -> Tensor3 a
update tensor [] = tensor
update (Tensor tensor) updates = Tensor $ tensor `updateT` updates
update (View tView) updates = View $ tView `updateView` updates

updateT :: Unbox a => T3 a -> [(I3, a)] -> T3 a
updateT tensor [] = tensor
updateT (T3 v sz) updates = T3 (V.unsafeUpd v $ map convert updates) sz
  where convert (idx, val) = iidx `seq` (iidx, val)
          where iidx = checkedLinearIdx sz idx

updateView :: Unbox a => T3View a -> [(I3, a)] -> T3View a
updateView tensor [] = tensor
updateView tensorView@(T3View {..}) updates = tensorView { tensor = tensor `updateT` map (first $ (closestIdx +) . (checkedIdx sizeView)) updates }

create :: Unbox a => Vector a -> Tensor3Size -> Tensor3 a
create v sz
  | V.length v == product sz = Tensor $ T3 v sz
  | otherwise = error "create: invalid tensor size"

createView :: Unbox a => T3 a -> BoundingBox -> T3View a
createView tensor@(T3 _ sz) (closestIdx, farthestIdx)
  | closestIdx >= (V3 0 0 0) &&
    closestIdx < farthestIdx &&
    farthestIdx < sz = let sizeView = farthestIdx - closestIdx + (V3 1 1 1) in T3View {..}
  | otherwise = error $ "createView: invalid bounding box" ++ " " ++ show closestIdx ++ ", " ++ show farthestIdx ++ " : " ++ show sz

replicate :: Unbox a => Tensor3Size -> a -> Tensor3 a
replicate sz v = Tensor $ replicateT sz v

replicateT :: Unbox a => Tensor3Size -> a -> T3 a
replicateT sz v = T3 (V.replicate (product sz) v) sz

indexing :: Tensor3Size -> [I3]
indexing (V3 xSize ySize zSize) = [(V3 x y z) | x <- [0..xSize-1], y <- [0..ySize-1], z <- [0..zSize-1]]

boundingBox :: Unbox a => Tensor3 a -> (a -> Bool) -> BoundingBox
boundingBox tensor pr = foldr helper (sz - (V3 1 1 1), V3 0 0 0) (indexing sz)
  where
    sz = size tensor
    helper idx bbox@(closest, farthest)
      | pr (tensor ! idx) = (min <$> idx <*> closest, max <$> idx <*> farthest)
      | otherwise = bbox

slice :: Unbox a => Tensor3 a -> BoundingBox -> Tensor3 a
slice (Tensor tensor) bbox = View $ createView tensor bbox
slice (View (T3View {..})) (closestNew, farthestNew)
  | closestNew >= (V3 0 0 0) &&
    farthestNew < sizeView = View $ createView tensor bbox
  | otherwise = error "slice: invalid bounding box"
    where
      bbox = (closestIdx + closestNew, closestIdx + farthestNew)

sliceAxis :: Unbox a => Tensor3 a -> Axis -> Int -> Int -> Tensor3 a
sliceAxis tensor X begin end = slice tensor (xBegin, xEnd)
  where
    xBegin = V3 begin 0 0
    (V3 _xsz ysz zsz) = size tensor
    xEnd = V3 end ysz zsz
sliceAxis tensor Y begin end = slice tensor (yBegin, yEnd)
  where
    yBegin = V3 0 begin 0
    (V3 xsz _ysz zsz) = size tensor
    yEnd = V3 xsz end zsz
sliceAxis tensor Z begin end = slice tensor (zBegin, zEnd)
  where
    zBegin = V3 0 0 begin
    (V3 xsz ysz _zsz) = size tensor
    zEnd = V3 xsz ysz end
{-
sliceAxis :: Unbox a => Tensor3 a -> Axis -> Int -> Int -> Tensor3 a
sliceAxis (Tensor tensor) axis = (View .) . (sliceAxisT tensor axis)
sliceAxis (View t3View) axis = (View .) . (sliceAxisView t3View axis)

sliceAxisT :: Unbox a => T3 a -> Axis -> Int -> Int -> T3View a
sliceAxisT tensor@(T3 _ (V3 _ ySize zSize)) X begin end = createView tensor (V3 begin 0 0, V3 end (ySize - 1) (zSize - 1))
sliceAxisT tensor@(T3 _ (V3 xSize _ zSize)) Y begin end = createView tensor (V3 0 begin 0, V3 (xSize - 1) end (zSize - 1))
sliceAxisT tensor@(T3 _ (V3 xSize ySize _)) Z begin end = createView tensor (V3 0 0 begin, V3 (xSize - 1) (ySize - 1) end)

sliceAxisView :: Unbox a => T3View a -> Axis -> Int -> Int -> T3View a
sliceAxisView (T3View {..}) X begin end = createView tensor (xBegin, xEnd)
  where
    xBegin = closestIdx + (V3 begin 0 0)
    xEnd = closestIdx + let (V3 _fx fy fz) = farthestIdx in (V3 end fy fz)
sliceAxisView (T3View {..}) Y begin end = createView tensor (yBegin, yEnd)
  where
    yBegin = closestIdx + (V3 0 begin 0)
    yEnd = closestIdx + let (V3 fx _fy fz) = farthestIdx in (V3 fx end fz)
sliceAxisView (T3View {..}) Z begin end = createView tensor (zBegin, zEnd)
  where
    zBegin = closestIdx + (V3 0 0 begin)
    zEnd = closestIdx + let (V3 fx fy _fz) = farthestIdx in (V3 fx fy end)
-}

nonzero :: Tensor3 Bool -> Int
nonzero (Tensor (T3 v _)) = V.length $ V.filter id v
nonzero tensor = foldr (\idx acc -> if tensor ! idx then acc + 1 else acc) 0 $ indexing (size tensor)

axisSize :: Tensor3 a -> Axis -> Int
axisSize tensor axis = axSize axis
  where
    (V3 xsz ysz zsz) = size tensor
    axSize X = xsz
    axSize Y = ysz
    axSize Z = zsz

fill :: Unbox a => Tensor3 a -> a -> Tensor3 a
fill tensor val = update tensor $ map (flip (,) val) $ indexing (size tensor)

fillBox :: Unbox a => Tensor3 a -> BoundingBox -> a -> Tensor3 a
fillBox t@(Tensor _) bbox val = Tensor $ tensor view
  where
    (View view) = slice t bbox `fill` val
fillBox (View view) bbox val = View view {tensor = tensor view'}
  where
    (View view') = slice (View view) bbox `fill` val

inBounds :: Tensor3 a -> I3 -> Bool
inBounds tensor = inSizeBounds (size tensor)

showY :: (Unbox a, Show a) => Int -> Tensor3 a -> String
showY y t = unlines $ map (\z -> concatMap (\x -> show $ t ! (V3 x y z)) [0..(w-1)]) [0..(d-1)] where
  (V3 w _h d) = size t

showZ :: (Unbox a, Show a) => Int -> Tensor3 a -> String
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

thawT :: (Unbox a, PrimMonad m) => T3 a -> m (MTensor3 (PrimState m) a)
thawT (T3 dat sz) = MT3 <$> MV.thaw dat <*> pure sz

thaw :: (Unbox a, PrimMonad m) => Tensor3 a -> m (MTensor3 (PrimState m) a)
thaw (Tensor t) = thawT t
thaw (View v) = thawT $ copyView v

freeze :: (Unbox a, PrimMonad m) => MTensor3 (PrimState m) a -> m (Tensor3 a)
freeze (MT3 dat sz) = Tensor <$> (T3 <$> MV.freeze dat <*> pure sz)

unsafeThawT :: (Unbox a, PrimMonad m) => T3 a -> m (MTensor3 (PrimState m) a)
unsafeThawT (T3 dat sz) = MT3 <$> MV.unsafeThaw dat <*> pure sz

unsafeThaw :: (Unbox a, PrimMonad m) => Tensor3 a -> m (MTensor3 (PrimState m) a)
unsafeThaw (Tensor t) = unsafeThawT t
unsafeThaw (View v) = unsafeThawT $ copyView v

unsafeFreeze :: (Unbox a, PrimMonad m) => MTensor3 (PrimState m) a -> m (Tensor3 a)
unsafeFreeze (MT3 dat sz) = Tensor <$> (T3 <$> MV.unsafeFreeze dat <*> pure sz)

read :: (Unbox a, PrimMonad m) => MTensor3 (PrimState m) a -> I3 -> m a
read (MT3 dat sz) idx = (lidx `seq`) <$> MV.unsafeRead dat lidx
  where lidx = checkedLinearIdx sz idx

write :: (Unbox a, PrimMonad m) => MTensor3 (PrimState m) a -> I3 -> a -> m ()
write (MT3 dat sz) idx val = MV.unsafeWrite dat lidx (lidx `seq` val)
  where lidx = checkedLinearIdx sz idx

mSize :: (Unbox a, PrimMonad m) => MTensor3 (PrimState m) a -> m Tensor3Size
mSize (MT3 _ sz) = return sz
