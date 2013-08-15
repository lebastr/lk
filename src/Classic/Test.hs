import Load
import Classic.LK
import Text.Printf
import Control.Monad

main = do
  ps <- loadData "/home/lebedev/Development/lk/data/sin.dat"
  forM_ (lk 0.05 0.1 (30*pi) ps) $ \(f,v) -> printf "%f %f\n" f v