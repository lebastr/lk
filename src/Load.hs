module Load where

import Control.Applicative
import System.IO

toFreq :: [(Double, Double)] -> [(Double, Double)]
toFreq ps = map (\(t,v) -> (1/t, v)) $ filter (\(t,_) -> t /= 0) $ map (\(t,v) -> (t-t0,v)) ps
  where
    t0 = minimum $ map fst ps

loadData :: FilePath -> IO [(Double, Double)]
loadData fname = (map ((\(x:y:_) -> (read x,read y)) . words) . lines) <$> readFile fname
  
fromStdin :: IO [(Double, Double)]
fromStdin = map ((\(x:y:_) -> (read x,read y)) . words) <$> loop where
  loop = do
    iseof <- isEOF
    case iseof of
      True -> return []
      False -> do
        line <- hGetLine stdin
        lines <- loop
        return (line:lines)