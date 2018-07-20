module ICFPC2018.IO
  ( getModel
  ) where

import Data.Word
import Data.Bits
import Linear.V3 (V3(..))
import qualified Data.Vector as V
import Data.Binary.Get

import ICFPC2018.Types
import qualified ICFPC2018.Tensor3 as T3

getBits :: Int -> Get [Bool]
getBits size = go size 0 0 []
  where go :: Int -> Int -> Word8 -> [Bool] -> Get [Bool]
        go 0 _ _ res = return $ reverse res
        go left 0 _ res = do
          newByte <- getWord8
          go left 8 newByte res
        go left byteLeft byte res = go (left - 1) (byteLeft - 1) (byte `shiftL` 1) (((byte .&. 1) == 1) : res)

getModel :: Get Model
getModel = do
  resolution <- fromIntegral <$> getWord8
  let size = resolution * resolution * resolution
  bitsData <- getBits size
  let vec = V.fromList bitsData
  return $ T3.create vec (V3 resolution resolution resolution)
