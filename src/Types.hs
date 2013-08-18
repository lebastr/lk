module Types where

newtype Freq = Freq { fromFreq :: Double } deriving (Show)

newtype Phase = Phase { fromPhase :: Double } deriving (Show)

type Mag = Double

newtype Epoch = Epoch { fromEpoch :: Double } deriving (Show)

