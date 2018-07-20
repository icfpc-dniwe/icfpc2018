module ICFPC2018.Scoring
  ( scoreCommand
  , scoreStep
  , scoreTrace
  ) where

import ICFPC2018.Types
import ICFPC2018.Tensor3
import ICFPC2018.Utils

scoreCommand :: Model -> Command -> Score
scoreCommand _ Halt = 0
scoreCommand _ Wait = 0
scoreCommand _ Flip = 0
scoreCommand _ (SMove lld) = 2 * mlen lld
scoreCommand _ (LMove sld1 sld2) = 2 * (clen sld1 + clen sld2 + 2)
scoreCommand _ (Fission _ _) = 24
scoreCommand m (Fill nd) = if m ! nd then 6 else 12
scoreCommand _ (FusionP _) = -24
scoreCommand _ (FusionS _) = 0

scoreStep :: Model -> Step -> Score
scoreStep m s = sum $ (scoreCommand m) <$> s

scoreTrace :: Model -> Trace -> Score
scoreTrace m t = sum $ (scoreStep m) <$> t