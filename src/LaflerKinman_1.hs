{- Классический алгоритм Lafler-Kinman -}

module LaflerKinman_1 where

import Data.List
import Numeric.GSL.Statistics
import Helper
import Data.Packed.Vector
import Types
import Data.Function
import Data.Array.MArray
import Control.Monad
import Data.Array.ST
import Data.Array

addValue :: (Int, Double) -> Double -> (Int, Double)
addValue (i0,v0) v = (i1,v1) where
  i1 = i0+1
  v1 = (v0*fromIntegral i0 + v) / fromIntegral i1

lkFreqs :: Int -> Phase -> (Freq, Freq) -> [(Epoch, Mag)] -> [(Freq, Double)]
lkFreqs zones phase_err (minFreq, maxFreq) curve = take 20 $
                                                   sortBy (\(_,a0) (_,a1) -> compare a1 a0)
                                                   [(f, varianceIndex f) | f <- freqs]
  where
    freqs = freqSeries phase_err latestEpoch (minFreq, maxFreq)
    latestEpoch = Epoch $ maximum $ map (fromEpoch.fst) curve
    varianceIndex freq = (fromIntegral total)*ncoeff / sum' [(a1-a0)^2 | (a0,a1) <- zip mags (tail mags)]
      where
        mags = map snd $ filter ((/=0) . fst) $ elems $ runSTArray $ do
          arr <- newArray (0,zones-1) (0,0.0)
          forM_ curve $ \(t,m) -> do
            let j = floor $ fromIntegral zones*fromPhase (toPhase freq t)
            v0 <- readArray arr j
            writeArray arr j (addValue v0 m)

          return arr


    total = length curve
    ncoeff = variance $ fromList $ map snd curve
