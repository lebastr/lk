{-# LANGUAGE OverloadedStrings #-}

import Load
import LK
import Text.Printf
import Control.Monad
import Control.Applicative
import System.IO
import Data.List
import Data.ByteString.Lazy (hPut)
import Data.Aeson
import Data.Aeson.Encode.Pretty

toFreq :: [(Double, Double)] -> [(Double, Double)]
toFreq ps = map (\(t,v) -> (1/t, v)) $ filter (\(t,_) -> t /= 0) $ map (\(t,v) -> (t-t0,v)) ps
  where
    t0 = minimum $ map fst ps

config = Config { confIndent = 4, confCompare = keyOrder ["Mean","Variance","freqs"] }

main = do
  ps <- toFreq <$> fromStdin
  let (xs, mX, mX2) = lkV 8 (1, 9) ps
  let json = object ["Mean" .= mX, "Variance" .= mX2, "freqs" .= xs]
  hPut stdout $ encodePretty' config json
  -- printf "Mean: %0.6f\tVariance: %0.6f\n" mX mX2
  -- putStrLn "-----------------"
  -- forM_ xs $ \(freq, v) -> printf "%0.6f\t%0.6f\n" freq v
