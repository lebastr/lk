module Load where

import Control.Applicative
import System.IO
import Control.Monad
import Text.Printf
import Types

linesFromStdin :: IO [String]
linesFromStdin = loop where
  loop = do
    iseof <- isEOF
    case iseof of
      True -> return []
      False -> do
        line <- hGetLine stdin
        lines <- loop
        return (line:lines)

linesFromFile :: FilePath -> IO [String]
linesFromFile name = lines <$> readFile name

lineToCurve :: String -> (Epoch, Mag)
lineToCurve = f . words 
  where
    f (x:y:_) = (Epoch (read x),read y)

curveFromStdin :: IO [(Epoch, Mag)]
curveFromStdin = map lineToCurve <$> linesFromStdin

curveFromFile :: FilePath -> IO [(Epoch, Mag)]
curveFromFile name = map lineToCurve <$> linesFromFile name

writeCurve :: FilePath -> [(Epoch, Mag)] -> IO ()
writeCurve name curve = do
  h <- openFile name WriteMode
  forM_ curve $ \(Epoch t, mag) -> hPutStr h $ printf "%f\t%f\n" t mag
  hClose h

printCurve :: [(Epoch, Mag)] -> IO ()
printCurve curve = forM_ curve $ \(Epoch t, mag) -> printf "%f\t%f\n" t mag

toFreq :: [(Double, Double)] -> [(Double, Double)]
toFreq ps = map (\(t,v) -> (1/t, v)) $ filter (\(t,_) -> t /= 0) $ map (\(t,v) -> (t-t0,v)) ps
  where
    t0 = minimum $ map fst ps

