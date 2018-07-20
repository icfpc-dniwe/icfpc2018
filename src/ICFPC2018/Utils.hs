module ICFPC2018.Utils where

import ICFPC2018.Types
import Linear.V3 (V3(..))

div1 :: Integral a => a -> a -> a
a `div1` b
  | m == 0 = d
  | otherwise = d + 1
  where (d, m) = a `divMod` b

infixl 7  `div1`

mlen :: Integral a => V3 a -> a
mlen (V3 x y z) = sum $ abs <$> [x, y, z]

clen :: Integral a => V3 a -> a
clen (V3 x y z) = foldr1 (\x y -> max x y) $ abs <$> [x, y, z]

mkLinearDifference :: Axis -> Int -> V3 Int
mkLinearDifference X v = V3 v 0 0
mkLinearDifference Y v = V3 0 v 0
mkLinearDifference Z v = V3 0 0 v


packMove :: VolatileCoordinate -> VolatileCoordinate -> [Command] 
packMove = error "TODO"

simulateStep :: VolatileCoordinate -> Command -> [VolatileCoordinate]
simulateStep = error "TODO"
