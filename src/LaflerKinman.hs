{- Классический алгоритм Lafler-Kinman -}

module LaflerKinman where

import Data.List
import Numeric.GSL.Statistics
import Helper
import Data.Packed.Vector
import Types
import Data.Function

lkFreqs :: Phase -> (Freq, Freq) -> [(Epoch, Mag)] -> [(Freq, Double)]
lkFreqs phase_err (minFreq, maxFreq) curve = take 20 $
                                             sortBy (\(_,a0) (_,a1) -> compare a1 a0)
                                             [(f, varianceIndex f) | f <- freqs]
  where
    freqs = freqSeries phase_err latestEpoch (minFreq, maxFreq)
    latestEpoch = Epoch $ maximum $ map (fromEpoch.fst) curve
    varianceIndex f = (fromIntegral total)*ncoeff / sum' [(a1-a0)^2 | (a0,a1) <- zip mags (tail mags)]
      where
        mags = map snd $ sortBy (compare `on` fst) [(fromPhase (toPhase f t), m) | (t,m) <- curve]

    total = length curve
    ncoeff = variance $ fromList $ map snd curve
