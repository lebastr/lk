module Load where

import Control.Applicative
import System.IO

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