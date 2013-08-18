{-# LANGUAGE OverloadedStrings #-}

import Text.Printf
import Load
import qualified LaflerKinman as LK0
import qualified LaflerKinman_1 as LK1
import Helper
import Control.Monad
import Types
import Challenge
import Text.Blaze.Html5
-- import Challenge


plotStats plotter curve stats = forM (stats) $ \(freq, md) -> do
  img <- plotter [(fromPhase (toPhase freq t), m) | (t,m) <- curve]
  return (freq, md, img)

main = runChallenge $ \plotter -> do
  curve <- curveFromStdin
  let stats = LK0.lkFreqs (Phase 0.05) (Freq 0.1, Freq 4) curve
  let stats1 = LK1.lkFreqs (Phase 0.05) (Freq 0.1, Freq 4) curve
  xs <- plotStats plotter curve stats
  xs1 <- plotStats plotter curve stats1
  return $ docTypeHtml $ do
    title "LK"
    body $ do
      table $ tr $ do
        td $ do
          p "First Algorithm"
      
          forM_ xs $ \(Freq f,v,i) -> do
            p $ toHtml $ (printf "Freq: %f. V: %f" f v :: String)
            img ! i

        td $ do
          p "Second Algorithm"
      
          forM_ xs1 $ \(Freq f,v,i) -> do
            p $ toHtml $ (printf "Freq: %f. V: %f" f v :: String)
            img ! i
      