module Load where

import Control.Applicative

loadData :: FilePath -> IO [(Double, Double)]
loadData fname = (map ((\(x:y:_) -> (read x,read y)) . words) . lines) <$> readFile fname
  