{-# LANGUAGE OverloadedStrings #-}

import Text.Printf
import Load
import qualified LaflerKinman as LK0
import qualified LaflerKinman_1 as LK1
import qualified LaflerKinman_2 as LK2
import Helper
import Control.Monad
import Types
import Challenge
import Text.Blaze.Html5 hiding (head)
import Control.Applicative
import System.Environment

plotStats plotter curve stats = forM (stats) $ \(freq, md) -> do
  img <- plotter (300,150) [(fromPhase (toPhase freq t), m) | (t,m) <- curve]
  return (freq, md, img)

renderCol xs = forM_ xs $ \(Freq f,v,i) -> do
  p $ toHtml $ (printf "Freq: %f. V: %f" f v :: String)
  img ! i


phase_err = 0.05
low_freq = 0.1
high_freq = 4

main = do
  fname <- head <$> getArgs     
  runChallenge def fname $ \plotter -> do
    curve <- curveFromFile fname
    let stats = LK0.lkFreqs (Phase phase_err) (Freq low_freq, Freq high_freq) curve
    let stats1 = LK1.lkFreqs 1000 (Phase phase_err) (Freq low_freq, Freq high_freq) curve
    let stats2 = LK1.lkFreqs 500 (Phase phase_err) (Freq low_freq, Freq high_freq) curve
    let stats3 = LK2.lkFreqs 1000 (Phase phase_err) (Freq low_freq, Freq high_freq) curve
    xs <- plotStats plotter curve stats
    xs1 <- plotStats plotter curve stats1
    xs2 <- plotStats plotter curve stats2
    xs3 <- plotStats plotter curve stats3
    return $ do
      h1 $ toHtml fname
      h2 "Parameters:"
      p $ toHtml $ "phase error: " ++ show phase_err
      p $ toHtml $ "low freq: " ++ show low_freq
      p $ toHtml $ "high freq: " ++ show high_freq
      table $ tr $ do
        td $ do
          p "First Algorithm"
          renderCol xs
        td $ do
          p "Second Algorithm 1000"
          renderCol xs1
        td $ do
          p "Third Algorithm 500"
          renderCol xs2
        td $ do
          p "Fourth Algorithm 1000"
          renderCol xs3
