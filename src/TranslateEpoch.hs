module Main where

import Load
import Types

toZeroEpoch :: [(Epoch, Mag)] -> [(Epoch, Mag)]
toZeroEpoch curve = map (\(Epoch t,v) -> (Epoch (t-t0),v)) curve
  where
    t0 = minimum $ map (fromEpoch.fst) curve

main = curveFromStdin >>= return . toZeroEpoch >>= printCurve