{-# LANGUAGE FlexibleContexts #-}

module Research.ChainMerge where

import Data.Array.MArray
import Control.Monad.ST.Lazy
import Data.Array.ST

type Freq = Double

freqSeries :: Freq -> [Freq]
freqSeries freq = [freq * fromIntegral i | i <- [0..]]

chainMerge :: [[Double]] -> [Double]
chainMerge xss = runST $ do
  let len = length xss
  arr <- newListArray (0,len-1) xss :: ST s (STArray s Int [Double])
  let chainLoop value n = chainLoop' n where
        chainLoop' n | n >= len = return []
                     | otherwise = do
          (v:vs) <- readArray arr n
          case v <= value of
            True -> do
              writeArray arr n vs
              vs' <- chainLoop' (n+1)
              return (v:vs')
            False -> chainLoop' (n+1)

  let loop = do
        (v0:vs@(v1:_)) <- readArray arr 0
        ps <- chainLoop v1 0
        let ps' = v0:ps
        pss <- loop
        return (ps:pss)

  pss <- loop
  return $ concat pss
  
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy cmp xs ys = merge xs ys where
  merge [] [] = []
  merge a@(x:xs) [] = a
  merge [] a@(x:xs) = a
  merge a@(x:xs) b@(y:ys) = case cmp x y of
    LT -> x : merge xs b
    EQ -> x : merge xs b
    GT -> y : merge a ys

mergeByS :: (a -> a -> Ordering) -> [[a]] -> [a]
mergeByS cmp xs = mergeS xs where
  mergeS [] = []
  mergeS (xs:xss) = mergeBy cmp xs $ mergeS xss
