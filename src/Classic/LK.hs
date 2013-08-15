module Classic.LK where

import Data.List
import Data.Function

type Point = (Double, Double)
type Freq = Double

sum' = foldl' (+) 0

frac :: Double -> Double
frac x = x - (fromIntegral $ floor x)

corrFactor :: Freq -> [Point] -> Double
corrFactor freq points = sum' [(v1-v0)^2 | (v0,v1) <- zip vs (tail vs)]
  where
    vs = map snd $ sortBy (compare `on` fst)
         [(frac (t*freq), v) | (t,v) <- points]

lk :: Double -> Freq -> Double -> [Point] -> [(Freq, Double)]
lk phase_err min_freq search_factor points = [(freq, 1/corrFactor freq points) |
                                              freq <- freqs]
  where
  freqs = [min_freq, min_freq+df..max_freq]
  max_freq = min_freq*search_factor
  df = phase_err/maximum (map fst points)