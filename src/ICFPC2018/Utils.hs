module ICFPC2018.Utils where

import Data.Foldable
import Data.Ord
import Control.Applicative
import Linear.V3 (V3(..))

div1 :: Integral a => a -> a -> a
a `div1` b
  | m == 0 = d
  | otherwise = d + 1
  where (d, m) = a `divMod` b

infixl 7  `div1`

inSizeBounds :: V3 Int -> V3 Int -> Bool
inSizeBounds (V3 xSize ySize zSize) (V3 xIdx yIdx zIdx)
  = xIdx < xSize && xIdx >= 0 &&
    yIdx < ySize && yIdx >= 0 &&
    zIdx < zSize && zIdx >= 0

{-
moving across Tensor3 starting from (0, 0, 0) along X first then Z and finally Y.
Y == 0 is bottom plane.
-}
snakeIdx :: V3 Int -> [V3 Int]
snakeIdx (V3 xSize ySize zSize) = helpFold (V3 0 0 0) False
  where
    helpFold (V3 xIdx yIdx zIdx) ySwitch
      | yIdx >= ySize = []
      | firstX = map xToV3 [0 .. xSize - 1] ++ changeZ (xSize - 1)
      | lastX = map xToV3 [xSize - 1, xSize - 2 .. 0] ++ changeZ 0
      | otherwise = undefined
      where
        firstX = xIdx == 0
        -- firstY = yIdx == 0
        firstZ = zIdx == 0
        lastX = xIdx == xSize - 1
        -- lastY = yIdx == ySize - 1
        lastZ = zIdx == zSize - 1
        xToV3 x = (V3 x yIdx zIdx)
        changeZ x | lastZ && not ySwitch = changeY x
                  | firstZ && ySwitch    = changeY x
                  | not ySwitch = helpFold (V3 x yIdx (zIdx + 1)) ySwitch
                  | ySwitch = helpFold (V3 x yIdx (zIdx - 1)) ySwitch
                  | otherwise = error "snakeIdx: impossible"
        changeY x = helpFold (V3 x (yIdx + 1) zIdx) (not ySwitch)

-- Manhattan distance
mlen :: Integral a => V3 a -> a
mlen v = sum $ abs <$> v

-- Chebyshev distance
clen :: Integral a => V3 a -> a
clen v = foldr1 max $ abs <$> v

linearPath :: V3 Int -> V3 Int -> [V3 Int]
linearPath from path
  | path == 0 = []
  | otherwise = next : linearPath next (path - step)
  where step = signum path
        next = from + step

mlenDistance :: Integral a => V3 a -> V3 a -> a
mlenDistance x x' = mlen (x - x')

infix 6 `mlenDistance`

-- Bounding box indices
boxIndices :: Enum a => V3 a -> V3 a -> [V3 a]
boxIndices (V3 xA yA zA) (V3 xB yB zB) = [ V3 x y z
                                         | x <- [xA..xB]
                                         , y <- [yA..yB]
                                         , z <- [zA..zB]
                                         ]

inBox :: Ord a => V3 a -> V3 a -> V3 a -> Bool
inBox (V3 xA yA zA) (V3 xB yB zB) (V3 x y z)
  = x <= xB && x >= xA &&
    y <= yB && y >= yA &&
    z <= zB && z >= zA

getBox :: Ord a => V3 a -> V3 a -> (V3 a, V3 a)
getBox (V3 x0 y0 z0) (V3 x1 y1 z1) = (b0, b1) where
  b0 = V3 (min x0 x1) (min y0 y1) (min z0 z1)
  b1 = V3 (max x0 x1) (max y0 y1) (max z0 z1)

splitPlanar :: (Eq a, Num a) => V3 a -> Maybe (V3 a, V3 a)
splitPlanar (V3 0 y z) = Just (V3 0 y 0, V3 0 0 z)
splitPlanar (V3 x 0 z) = Just (V3 x 0 0, V3 0 0 z)
splitPlanar (V3 x y 0) = Just (V3 x 0 0, V3 0 y 0)
splitPlanar _ = Nothing

maybeAlt :: Alternative f => Maybe a -> f a
maybeAlt = maybe empty pure

argmax :: (Ord a, Foldable t) => t a -> Int
argmax elems = fst $ maximumBy (comparing snd) $ zip [0..] $ toList elems
