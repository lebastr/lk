module Modern.LK where

import Data.List
import Data.Function

type Point = (Double, Double)
type Freq = Double
type Time = Double

freqSeries :: Int -> Freq -> Time -> [Freq]
freqSeries n freq time = [freq0, freq0+df..] where
  freq0 = fromIntegral (floor (freq*time)) / time
  df = 1 / fromIntegral n / time

chain :: Ord a => [a] -> [a]
chain [] = []
chain [x] = [x]
chain (x:y:ps) | x <= y = x:chain (y:ps)
               | otherwise = [x]

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge a@(x:xs) [] = a
merge [] a@(x:xs) = a
merge a@(x:xs) b@(y:ys) | x <= y = x : merge xs b
                        | otherwise = y : merge a ys

mergeS :: Ord a => [[a]] -> [a]
mergeS [] = []
mergeS (xs:xss) = merge xs $ mergeS xss

freqSeriesWithIndex :: Int -> Freq -> Time -> [(Int, Freq)]
freqSeriesWithIndex n f t = zip (cycle [0..n-1]) $ freqSeries n f t

insertV0 :: Double -> (Int, Double) -> (Int, Double)
insertV0 v (i,mean) = (i',mean') where
  i' = i+1
  mean' = (fromIntegral i * mean+v)/fromIntegral i'

insertV :: Double -> (Int, Double, Double) -> (Int, Double, Double)
insertV p (i,mean,variance) = (i',mean',variance')
  where
    (i',mean') = insertV0 p (i,mean)
    variance' = snd (insertV0 (p^2) (i,variance+mean^2)) - mean'^2

deleteV0 :: Double -> (Int, Double) -> (Int, Double)
deleteV0 v (i,mean) = (i',mean') where
  i' = i-1
  mean' = (fromIntegral i * mean-v)/fromIntegral i'

deleteV :: Double -> (Int, Double, Double) -> (Int, Double, Double)
deleteV p (i,mean,variance) = (i',mean',variance')
  where
    (i',mean') = deleteV0 p (i,mean)
    variance' = snd (deleteV0 (p^2) (i,variance+mean^2)) - mean'^2

cell :: Int -> Time -> Freq -> Int
cell n time freq = floor (freq*time*fromIntegral n) `mod` n

mean :: [Double] -> Double
mean = undefined

variance :: [Double] -> Double
variance = undefined