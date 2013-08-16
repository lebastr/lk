import Load
import Research.LK
import Research.ChainMerge
import Control.Applicative
import Data.List

main = do
    ps <- sortBy (\(t0,_) (t1,_) -> compare t1 t0) <$> loadData "/home/lebedev/Development/lk/data/sin.dat"
    let seqs = map (freqSeries 1 (1/30/pi) . fst) ps
        xs = takeWhile ((<=0.1)) $ mergeS $ seqs
        ys = takeWhile ((<=0.1)) $ chainMerge $ seqs
    writeFile "qq1.txt" $ show xs
    writeFile "qq2.txt" $ show ys
