module ICFPC2018.IO
  ( getModel
  , putTrace
  ) where

import Data.Word
import Data.Bits
import Linear.V3 (V3(..))
import qualified Data.Vector as V
import Data.Binary.Get
import Data.Binary.Put

import Debug.Trace
import ICFPC2018.Types
import qualified ICFPC2018.Tensor3 as T3

getBits :: Int -> Get [Bool]
getBits size = go size 0 0 []
  where go :: Int -> Int -> Word8 -> [Bool] -> Get [Bool]
        go 0 _ _ res = return $ reverse res
        go left 0 _ res = do
          newByte <- getWord8
          go left 8 newByte res
        go left byteLeft byte res = go (left - 1) (byteLeft - 1) (byte `shiftR` 1) (((byte .&. 1) == 1) : res)

getModel :: Get Model
getModel = do
  resolution <- fromIntegral <$> getWord8
  let size = resolution * resolution * resolution
  bitsData <- getBits size
  let vec = V.fromList bitsData
  return $ T3.create vec (V3 resolution resolution resolution)

encodeShortDifference :: ShortDifference -> (Word8, Word8)
encodeShortDifference (V3 x 0 0) = (0b01, fromIntegral x + 5)
encodeShortDifference (V3 0 y 0) = (0b10, fromIntegral y + 5)
encodeShortDifference (V3 0 0 z) = (0b11, fromIntegral z + 5)
encodeShortDifference _ = error "encodeShortDifference: impossible"

encodeLongDifference :: LongDifference -> (Word8, Word8)
encodeLongDifference (V3 x 0 0) = (0b01, fromIntegral x + 15)
encodeLongDifference (V3 0 y 0) = (0b10, fromIntegral y + 15)
encodeLongDifference (V3 0 0 z) = (0b11, fromIntegral z + 15)
encodeLongDifference _ = error "encodeLongDifference: impossible"

encodeNearDifference :: NearDifference -> Word8
encodeNearDifference (V3 x y z) = fromIntegral $ (x + 1) * 9 + (y + 1) * 3 + (z + 1)

putCommand :: Command -> Put
putCommand Halt = putWord8 0b11111111
putCommand Wait = putWord8 0b11111110
putCommand Flip = putWord8 0b11111101
putCommand (SMove lld) = putWord8 byte1 >> putWord8 byte2
  where (a, i) = encodeLongDifference lld
        byte1 = 0b0100 .|. (a `shiftL` 4)
        byte2 = i
putCommand (LMove sld1 sld2) = putWord8 byte1 >> putWord8 byte2
  where (a1, i1) = encodeShortDifference sld1
        (a2, i2) = encodeShortDifference sld2
        byte1 = 0b1000 .|. (a1 `shiftL` 4) .|. (a2 `shiftL` 6)
        byte2 = i1 .|. (i2 `shiftL` 4)
putCommand (Fission nd m) = putWord8 byte1 >> putWord8 byte2
  where end = encodeNearDifference nd
        byte1 = 0b101 .|. (end `shiftL` 3)
        byte2 = fromIntegral m
putCommand (Fill nd) = putWord8 byte
  where end = encodeNearDifference nd
        byte = 0b011 .|. (end `shiftL` 3)
putCommand (FusionP nd) = putWord8 byte
  where end = encodeNearDifference nd
        byte = 0b111 .|. (end `shiftL` 3)
putCommand (FusionS nd) = putWord8 byte
  where end = encodeNearDifference nd
        byte = 0b110 .|. (end `shiftL` 3)

putTrace :: Trace -> Put
putTrace = mapM_ (mapM_ putCommand . map snd . M.toAscList)
