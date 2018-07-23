module ICFPC2018.Prepare where

import Linear.V3 (V3(..))

import ICFPC2018.Types
import qualified ICFPC2018.Tensor3 as T3

sliceModel :: Model -> T3.Axis -> [Model]
sliceModel model axis = map (\(begin, end) -> T3.sliceAxis model axis begin (end - 1)) boundings
  where
    (V3 xsz ysz zsz) = T3.size model
    axisSize T3.X = xsz
    axisSize T3.Y = ysz
    axisSize T3.Z = zsz
    sz = axisSize axis
    indices = [0, maxFD .. (sz - 2)] ++ [sz]
    boundings = zip (init indices) (tail indices)

splitModel :: Model -> T3.Axis -> (Model, Model)
splitModel model axis = (subModel (0, splitIdx - 1), subModel (splitIdx, sz))
  where
    (V3 xsz ysz zsz) = T3.size model
    axisSize T3.X = xsz
    axisSize T3.Y = ysz
    axisSize T3.Z = zsz
    sz = axisSize axis
    step = 5
    indices = [2, step .. (sz - 2)]
    subModel (begin, end) = T3.sliceAxis model axis begin (end - 1)
    heuristicsFirst = map (bboxHeuristics . subModel) $ map ((,) 0) indices
    heuristicsSecond = map (bboxHeuristics . subModel) $ map (flip (,) sz) indices
    heuristics = zipWith (\x y -> abs $ x - y) heuristicsFirst heuristicsSecond
    argmax elems = snd $ foldr1 findMax $ zip elems [1..]
      where
        findMax f@(val, _) m@(maxVal, _) | maxVal < val = f
                                                | otherwise = m
    splitIdx = indices !! (argmax heuristics)

bboxHeuristics :: Model -> Double
bboxHeuristics model = sm / sz
  where
    bbox = T3.boundingBox model id
    subModel = T3.slice model bbox
    modelSum :: Int
    modelSum = T3.nonzero subModel
    sm :: Double
    sm = fromIntegral modelSum
    sz :: Double
    sz = fromIntegral $ product $ T3.size subModel
