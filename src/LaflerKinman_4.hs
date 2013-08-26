{- Классический алгоритм Lafler-Kinman -}

module LaflerKinman_4 where

import Data.List
import qualified Data.Vector.Storable as S
import CLaflerKinman
import Types
import Helper
import System.IO.Unsafe
import Control.Parallel.Strategies

splitByN n ps = loop ps where
  loop ps = let (a,as) = splitAt n ps
            in case null as of
              True -> [a]
              False -> a : loop as


lkFreqs :: Phase -> (Freq, Freq) -> [(Epoch, Mag)] -> [(Freq, Double)]
lkFreqs phase_err (minFreq, maxFreq) curve = take 20 $
                                             sortBy (\(_,a0) (_,a1) -> compare a1 a0) $
                                             map (\p -> (Freq (vFreq p), vIdx p)) $ 
                                             S.toList $ S.concat $ 
                                             parMap rdeepseq 
                                             (\freqs -> runLaflerKinman freqs obs) freqss
  where
    freqss = let list = map fromFreq $ freqSeries phase_err latestEpoch (minFreq, maxFreq)
             in map S.fromList $ splitByN (length list `div` 7) list

    latestEpoch = Epoch $ maximum $ map (fromEpoch.fst) curve
    obs = S.fromList $ map (\(Epoch t, m) -> Observation t m) curve