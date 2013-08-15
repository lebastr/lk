import Control.Monad
import System.Random
import Text.Printf
import Data.List

type Amplitude = Double
type Phase = Double
type Freq = Double
type Point = (Double, Double)

type Harmonic = (Freq, Amplitude, Phase)

sum' xs = foldl' (+) 0 xs

addNoise :: Double -> [Point] -> IO [Point]
addNoise delta ps = forM ps $ \(x,y) -> do
  dp <- randomRIO (-delta, delta)
  return (x, y+dp)

harmSum :: [Harmonic] -> Double -> Double
harmSum harms x = sum' [a*sin (f*x+p) | (f,a,p) <- harms]

gRD :: Int -> (Double, Double) -> (Double -> Double) -> IO [Point]
gRD n (xmax, xmin) f = do
  xs <- replicateM n $ randomRIO (xmin, xmax)
  return [(x, f x) | x <- xs]

simpleHarmonic :: Int -> (Double, Double) -> IO [Point]
simpleHarmonic n b = gRD n b sin

harmonic :: Int -> (Double, Double) -> [Harmonic] -> IO [(Double, Double)]
harmonic n bound harms = gRD n bound (harmSum harms)

printData :: [(Double, Double)] -> IO ()
printData ps = forM_ ps $ \(x,y) -> printf "%f %f\n" x y
