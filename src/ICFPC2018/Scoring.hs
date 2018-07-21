module ICFPC2018.Scoring
  ( scoreCommand
  , scoreStep
  , scoreTrace
  ) where

import qualified Data.Vector as V
import Linear.V3 (V3(..))
import Debug.Trace

import ICFPC2018.Types
import qualified ICFPC2018.Tensor3 as T3
import ICFPC2018.Utils

scoreCommand :: Model -> Command -> Score
scoreCommand _ Halt = 0
scoreCommand _ Wait = 0
scoreCommand _ Flip = 0
scoreCommand _ (SMove lld) = 2 * mlen lld
scoreCommand _ (LMove sld1 sld2) = 2 * (clen sld1 + clen sld2 + 2)
scoreCommand _ (Fission _ _) = 24
scoreCommand m (Fill nd) = if m T3.! nd then 6 else 12
scoreCommand _ (FusionP _) = -24
scoreCommand _ (FusionS _) = 0

stepFlipsHarmonic :: Step -> Bool
stepFlipsHarmonic s = (len `mod` 2) /= 0 where
    len = length $ filter (
        \b -> case b of
            Flip -> True
            _    -> False
        ) $ V.toList s

scoreStep :: Model -> Step -> (Score, Bool) {-(Score, HarmonicFlip)-}
scoreStep m s = (score, stepFlipsHarmonic s) where
    score = localCost + (sum $ (scoreCommand m) <$> s)
    localCost = 20 * length s

scoreTrace' :: HarmonicState -> Model -> Trace -> Score
scoreTrace' _ _ [] = 0
scoreTrace' harmonicState model (h:t) = trace ("h: " ++ show h ++ " harm: " ++ show newHarmonicState ++ " gs: " ++ show globalCost ++ " ts: " ++ show totalStepScore) $ globalCost + totalStepScore + scoreTrace' harmonicState model t where
    V3 r _ _ = T3.size model
    stepScore = scoreStep model h
    totalStepScore = fst stepScore
    flipHarmonic = snd stepScore
    newHarmonicState = if flipHarmonic then changeHarmonic harmonicState else harmonicState
    globalCost = harmonicCost * r ^ (3 :: Int)
    harmonicCost = case newHarmonicState of
      High -> 30
      _    -> 3

scoreTrace :: Model -> Trace -> Score
scoreTrace model t
  | xS == yS && yS == zS = scoreTrace' Low model t
  | otherwise = error "scoreTrace: invalid model"
  where V3 xS yS zS = T3.size model
