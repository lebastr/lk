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


config = Config { confIndent = 4, confCompare = keyOrder ["Mean","Variance","freqs"] }

main = do
  ps <- toFreq <$> fromStdin
  let (xs, mX, mX2) = lkV 8 (1/4, 4) $ clip snd ps
  let json = object ["Mean" .= mX, "Variance" .= mX2, "freqs" .= xs]
  hPut stdout $ encodePretty' config json
  -- printf "Mean: %0.6f\tVariance: %0.6f\n" mX mX2
  -- putStrLn "-----------------"
  -- forM_ xs $ \(freq, v) -> printf "%0.6f\t%0.6f\n" freq v
