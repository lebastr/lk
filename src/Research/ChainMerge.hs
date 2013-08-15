{-# LANGUAGE FlexibleContexts #-}

module Modern.ChainMerge where

import Data.Array.MArray
import Control.Monad.ST.Lazy
import Data.Array.ST

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
            False -> return []

  let loop = do
        (v0:vs@(v1:_)) <- readArray arr 0
        ps <- chainLoop v1 0
        let ps' = v0:ps
        pss <- loop
        return (ps:pss)

  pss <- loop
  return $ concat pss