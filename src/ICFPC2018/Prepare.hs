module ICFPC2018.Prepare where

import ICFPC2018.Types
import ICFPC2018.Utils
import qualified ICFPC2018.Tensor3 as T3

sliceModel :: Model -> T3.Axis -> [Model]
sliceModel model axis = map (\(begin, end) -> T3.sliceAxis model axis begin (end - 1)) boundings
  where
    sz = T3.axisSize model axis
    indices = [0, maxFD .. (sz - 2)] ++ [sz]
    boundings = zip (init indices) (tail indices)

splitModel :: Model -> T3.Axis -> (Model, Model)
splitModel model axis = (subModel 0 (splitIdx - 1), subModel splitIdx sz)
  where
    sz = T3.axisSize model axis
    step = 5
    indices = [2, step .. (sz - 2)]
    subModel begin end = T3.sliceAxis model axis begin (end - 1)

    part :: Int -> Int -> Double
    part begin end = fromIntegral (end - begin) / (fromIntegral sz)

    heuristics (begin, end) = (part begin end) * (bboxHeuristics . subModel begin $ end)
    heuristicsFirst = map heuristics $ map ((,) 0) indices
    heuristicsSecond = map heuristics $ map (flip (,) sz) indices
    combinedHeuristics = zipWith (\x y -> abs $ x - y) heuristicsFirst heuristicsSecond
    splitIdx = indices !! (argmax combinedHeuristics)

bboxHeuristics :: Model -> Double
bboxHeuristics model = sm / sz
  where
    bbox = T3.boundingBox model id
    subModel = T3.slice model bbox

    sm :: Double
    sm = fromIntegral $ T3.nonzero subModel

    sz :: Double
    sz = fromIntegral $ product $ T3.size subModel
