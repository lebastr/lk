{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Control.Parallel.Strategies

newtype Freq = Freq { fromFreq :: Double } deriving (Show, NFData)

newtype Phase = Phase { fromPhase :: Double } deriving (Show)

type Mag = Double

newtype Epoch = Epoch { fromEpoch :: Double } deriving (Show)

