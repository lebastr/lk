module Classic.LK where

import Data.List
import Data.Function
import Data.Packed.Vector
import Numeric.GSL.Statistics
import Data.Array

type Point = (Freq, Double)
type Freq = Double
type Phase = Double


sum' = foldl' (+) 0

frac :: Double -> Double
frac x = x - (fromIntegral $ floor x)


roundF :: Double -> Double -> Double -> Double
roundF dx x0 x1 = dx * fromIntegral (floor ((x1 - x0)/dx)) + x0

freqSeries :: Phase -> Freq -> (Freq, Freq) -> [Freq]
freqSeries phase_err low_freq (min_freq, max_freq) = [min_freq', min_freq'+df..max_freq']
  where
    min_freq' = roundF df low_freq min_freq
    max_freq' = roundF df low_freq max_freq
    df = low_freq * phase_err

countLK :: Phase -> (Freq, Freq) -> [Point] -> Int
countLK phase_err (min_freq, max_freq) ps = length freqs where
  freqs = freqSeries phase_err low_freq (min_freq, max_freq)
  low_freq = minimum $ map fst ps

lk :: Phase -> (Freq, Freq) -> [Point] -> [(Freq, Double)]
lk phase_err (min_freq, max_freq) ps = [(f, 1/corrFactor f ps) | f <- freqs] where
  freqs = freqSeries phase_err low_freq (min_freq, max_freq)
  low_freq = minimum $ map fst ps

corrFactor :: Freq -> [Point] -> Double
corrFactor freq_0 ps = sum' [(v1-v0)^2 | (v0,v1) <- zip vs (tail vs)]
  where
    vs = map snd $ sortBy (compare `on` fst)
         [(frac (freq_0/freq), v) | (freq,v) <- ps]

lkV :: Int -> (Freq, Freq) -> [Point] -> [(Freq, Double)]
lkV nZones (min_freq, max_freq) ps = [(f, 1/var nZones f ps) | f <- freqs] 
  where
    phase_err = 1 / fromIntegral nZones
    freqs = freqSeries phase_err low_freq (min_freq, max_freq)
    low_freq = minimum $ map fst ps

factorize :: Freq -> Freq -> Phase
factorize freq0 freq = frac (freq0/freq)

var :: Int -> Freq -> [Point] -> Double
var nZones freq0 ps = sum' variances
  where
    variances = [getVar (arr!i) | i <- [0..nZones-1]]
    getVar ps | null ps = 0
              | otherwise = variance $ fromList ps
    nZ = fromIntegral nZones
    zone freq = floor (nZ*factorize freq0 freq) `mod` nZones
    arr = accumArray (\e a -> a:e) [] (0,nZones-1) 
          [(zone freq, val) | 
              (freq, val) <- ps]
