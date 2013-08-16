import Load
import Classic.LK
import Text.Printf
import Control.Monad
import Control.Applicative
import System.IO
import Data.List
-- import 

toFreq :: [(Double, Double)] -> [(Double, Double)]
toFreq ps = map (\(t,v) -> (1/t, v)) $ filter (\(t,_) -> t /= 0) $ map (\(t,v) -> (t-t0,v)) ps
  where
    t0 = minimum $ map fst ps

main = do
  ps <- toFreq <$> loadData "/home/lebedev/Development/lk/data/curve.dat"

--  print $ countLK 0.05 (0.5, 3) ps
  let xs = lk 0.05 (1, 9) ps
      ys = take 10 $ sortBy (\(_,v0) (_,v1) -> compare v1 v0) xs

  print $ countLK 0.05 (1, 9) ps
  forM_ ys $ \(freq, mag) -> do
    let fname = printf "%0.4f_%0.4f.dat" freq mag
    fn <- openFile fname WriteMode
    forM_ ps $ \(f,v) -> do
      let p = factorize freq f
      hPutStrLn fn $ printf "%f  %f" p v
    hClose fn
