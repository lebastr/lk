{- Классический алгоритм Lafler-Kinman -}

module LaflerKinman_2 where

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

lkFreqs :: Int -> Phase -> (Freq, Freq) -> [(Epoch, Mag)] -> [(Freq, Double)]
lkFreqs zones phase_err (minFreq, maxFreq) curve = take 20 $
                                                   sortBy (\(_,a0) (_,a1) -> compare a1 a0)
                                                   [(f, varianceIndex f) | f <- freqs]
  where
    freqs = freqSeries phase_err latestEpoch (minFreq, maxFreq)
    latestEpoch = Epoch $ maximum $ map (fromEpoch.fst) curve
    varianceIndex freq = (fromIntegral total)*ncoeff / sum' [(a1-a0)^2 | (a0,a1) <- zip mags (tail mags)]
      where
        mags = map (\(Just s) -> s) $ filter (\s -> case s of
                                                 Nothing -> False
                                                 _ -> True) $ elems $ runSTArray $ do
          arr <- newArray (0,zones-1) Nothing
          forM_ curve $ \(t,m) -> do
            let j = floor $ fromIntegral zones*fromPhase (toPhase freq t)
            let f = if j `mod` 2 == 0 then max else min
            m0 <- readArray arr j
            case m0 of
              Nothing -> writeArray arr j (Just m)
              Just m0 -> writeArray arr j (Just $ f m0 m)

          return arr


    total = length curve
    ncoeff = variance $ fromList $ map snd curve
