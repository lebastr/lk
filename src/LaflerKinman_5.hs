{- Классический алгоритм Lafler-Kinman -}

module LaflerKinman_5 where

import Types
import Helper
import qualified Data.Vector.Unboxed as U
import Data.List
import Control.Parallel.Strategies


lkFreqs :: Phase -> (Freq, Freq) -> [(Epoch, Mag)] -> [(Freq, Double)]
lkFreqs phase_err freqBounds obs = laflerKinmanVec phase_err freqBounds $ U.fromList $
                                   map (\(Epoch t, m) -> (t,m)) obs


laflerKinmanVec :: Phase -> (Freq, Freq)
                   -> U.Vector (Double, Mag)
                   -> [(Freq, Double)]
laflerKinmanVec phase_err freqBounds observation = idxList
  where
    idxList = take 40 $ sortBy (\(_,a0) (_,a1) -> compare a1 a0) $
              withStrategy (parListChunk 1024 rdeepseq) $
              map (\f -> (f, varianceIndex f)) freqs

    freqs = freqSeries phase_err lEp freqBounds
      where lEp = Epoch $ U.maximum $ U.map fst observation

    nObs = U.length observation

    varianceIndex freq = (sx_2 - sx^2 / total) / sumD where
      (sx_2, sx, total, sumD) = {-# SCC foldl #-} U.foldl' gFold (0,0,0,0) $
                                U.zip valVec (U.tail valVec)

      valVec = U.map snd $ U.filter fst $
               {-# SCC accumulate #-} U.accumulate ({-# SCC accF #-} accF) (emptyVec) $
               {-# SCC mapScatter #-} U.map (\(t,m) -> let j = scatter t in (j,(j,m))) observation

      scatter :: Double -> Int
      scatter t = 1 + floor' (fromIntegral nObs * fromPhase (toPhase freq $ Epoch t))
      {-# INLINE scatter #-}

      accF (False,_) (_, v) = {-# SCC trueAccF #-} (True, v)
      accF (True,v0) (j, v1) = {-# SCC falseAccF #-} case j `mod` 2 == 0 of
        True -> (True, max v0 v1)
        False -> (True, min v0 v1)

      {-# INLINE accF #-}

      emptyVec :: U.Vector (Bool, Mag)
      emptyVec = {-# SCC emptyVec #-} U.replicate (nObs+1) (False,0)

      gFold (sx_2,sx,total,sumD) (v0,v1) = (sx_2+v0^2,sx+v0,total+1,sumD+(v1-v0)^2)
      {-# INLINE gFold #-}

