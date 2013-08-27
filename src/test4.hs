import Types
import qualified LaflerKinman_4 as LK4
import Load

phase_err = 0.05
low_freq = 0.1
high_freq = 10


main = do
  let fname = "/home/lebedev/Development/lk/data/assas/000108-3330.1.dat"
  curve <- curveFromFile fname
  print $ take 10 $ LK4.lkFreqs (Phase phase_err) (Freq low_freq, Freq high_freq) curve
      
