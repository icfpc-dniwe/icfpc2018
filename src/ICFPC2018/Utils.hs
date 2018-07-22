module ICFPC2018.Utils where

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

{-# INLINE linearPath #-}
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
