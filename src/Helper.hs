module Helper where

import Types
import Data.List

sum' :: [Double] -> Double
sum' = foldl' (+) 0

-- frac x == {x}

frac :: Double -> Double
frac x = x - (fromIntegral $ floor x)

-- Переводит период в частоту

toFreq :: Epoch -> Freq
toFreq (Epoch t) = Freq $ 1/t

-- toPhase freq epoch сворачиват по частоте freq наблюдение с эпохой epoch

toPhase :: Freq -> Epoch -> Phase
toPhase (Freq freq) (Epoch epoch) = Phase $ frac $ freq*epoch

{- freqSeries phase_err epoch (minFreq, maxFreq) порождает последовательность частот,
   для каждой из которой свертка epoch с ней приводит к фиксированному сдвину по фазе phase_err. -}

freqSeries :: Phase -> Epoch -> (Freq, Freq) -> [Freq]
freqSeries phase_err epoch (Freq min_freq, Freq max_freq) = map Freq [min_freq', min_freq'+df..max_freq']
  where
    low_freq = fromFreq $ toFreq epoch
    min_freq' = roundF df low_freq min_freq
    max_freq' = roundF df low_freq max_freq
    df = low_freq * fromPhase phase_err
    roundF dx x0 x1 = dx * fromIntegral (floor ((x1 - x0)/dx)) + x0
