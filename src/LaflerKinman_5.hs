{- Классический алгоритм Lafler-Kinman -}
{-# LANGUAGE BangPatterns #-}

module LaflerKinman_5 where

import Types
import Helper
import qualified Data.Vector.Unboxed as U
import Data.List
import Control.Parallel.Strategies
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Fusion.Stream as STR
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Vector.Mutable (STVector)
import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Algorithms.Intro as I
import Debug.Trace

accF :: (Bool, Double) -> (Int, Double) -> (Bool, Double)
accF (False, _) (_,v) = (True, v)
accF (True, v0) (j,v1) = case j `mod` 2 == 0 of
    True -> (True, max v0 v1)
    False -> (True, min v0 v1)

{-# INLINE accF #-}

gFold (!sx_2,!sx,!total,!sumD) (!v0,!v1) = (sx_2+v0^2,sx+v0,total+1,sumD+(v1-v0)^2)
{-# INLINE gFold #-}


splitByN n ps = loop ps where
  loop ps = let (a,as) = splitAt n ps
            in case null as of
              True -> [a]
              False -> a : loop as


lkFreqs :: Phase -> (Freq, Freq) -> [(Epoch, Mag)] -> [(Freq, Double)]
lkFreqs phase_err freqBounds obs = laflerKinmanVec phase_err freqBounds $ U.fromList $
                                   map (\(Epoch t, m) -> (t,m)) obs

laflerKinmanVec :: Phase -> (Freq, Freq)
                   -> U.Vector (Double, Mag)
                   -> [(Freq, Double)]
laflerKinmanVec phase_err freqBounds observation = map (\(f,v) -> (Freq f, v)) $ take 40 $ 
                                                   sortBy (\(_,a0) (_,a1) -> compare a1 a0) $
                                                   concat $ 
                                                   parMap rdeepseq (\fs -> idxList fs) freqss
  where
    idxList freqs = G.toList $ runST $ do
                      mVec <- GM.replicate (nObs+1) (False, 0) :: ST s (UM.STVector s (Bool, Double))
                      iVec <- GM.new $ length freqs :: ST s (UM.STVector s (Double, Double))
                      forM_ (zip [0..] freqs) $ \(!j,!f) -> do
                        !v <- varianceIndex mVec f
                        GM.unsafeWrite iVec j (fromFreq f, v)

                      v <- G.basicUnsafeFreeze iVec
                      return (v :: U.Vector (Double, Double))

    freqss = 
      let lEp = Epoch $ U.maximum $ U.map fst observation
          xs = freqSeries phase_err lEp freqBounds 
      in splitByN (length xs `div` 7) xs

    nObs = U.length observation

--     varianceIndex :: STVector s (Bool, Double) -> Freq -> ST s Double
    varianceIndex !mVec !freq = do
       GM.unsafeAccum accF mVec $
         G.stream $ G.map (\(t,m) -> let j = scatter t in (j,(j,m))) observation

       emptyVec <- G.basicUnsafeFreeze mVec
       let valVec = STR.map snd $ STR.filter fst $ G.stream (emptyVec :: U.Vector (Bool, Double))
       let (sx_2, sx, total, sumD) = STR.foldl' gFold (0,0,0,0) $
                                     STR.zip valVec (STR.tail valVec)

       total `seq` (GM.set mVec (False, 0))

       return $ (sx_2 - sx^2 / total) / sumD

      where
        scatter :: Double -> Int
        scatter = \t -> 1 + floor' (fromIntegral nObs * fromPhase (toPhase freq $ Epoch t))
        {-# INLINE scatter #-}

