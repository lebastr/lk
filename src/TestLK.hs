{-# LANGUAGE OverloadedStrings #-}

import Text.Printf
import Load
import qualified LaflerKinman as LK0
import qualified LaflerKinman_1 as LK1
import Helper
import Control.Monad
import Types
import Challenge
import Text.Blaze.Html5 hiding (head)
import Control.Applicative
import System.Environment

plotStats plotter curve stats = forM (stats) $ \(freq, md) -> do
  img <- plotter (400,200) [(fromPhase (toPhase freq t), m) | (t,m) <- curve]
  return (freq, md, img)

renderCol xs = forM_ xs $ \(Freq f,v,i) -> do
  p $ toHtml $ (printf "Freq: %f. V: %f" f v :: String)
  img ! i


main = do
  fname <- head <$> getArgs     
  runChallenge def fname $ \plotter -> do
    curve <- curveFromFile fname
    let stats = LK0.lkFreqs (Phase 0.05) (Freq 0.1, Freq 4) curve
    let stats1 = LK1.lkFreqs 1000 (Phase 0.05) (Freq 0.1, Freq 4) curve
    let stats2 = LK1.lkFreqs 500 (Phase 0.05) (Freq 0.1, Freq 4) curve
    xs <- plotStats plotter curve stats
    xs1 <- plotStats plotter curve stats1
    xs2 <- plotStats plotter curve stats2
    return $ do
      h1 $ toHtml fname
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
