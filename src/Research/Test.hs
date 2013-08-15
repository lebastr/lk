import Load
import Modern.LK
import Modern.ChainMerge

main = do
    ps <- loadData "/home/lebedev/Development/lk/data/sin.dat"
    let seqs = map (freqSeries 1 (1/30/pi) . fst) ps
        xs = takeWhile ((<=0.1)) $ mergeS $ seqs
        ys = takeWhile ((<=0.1)) $ chainMerge $ seqs
    writeFile "qq.txt" $ show xs
    writeFile "qq1.txt" $ show ys