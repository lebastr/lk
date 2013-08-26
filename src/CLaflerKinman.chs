module CLaflerKinman where

import Control.Applicative
import Control.Monad
import Foreign.Storable
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import qualified Data.Vector.Generic as G
import Foreign.ForeignPtr.Safe
import Foreign.C.Types
import Foreign.Ptr
import System.IO.Unsafe

data Observation = Observation {
      oTime :: Double
    , oValue :: Double }

data Bin = Bin {
      emptyFlag :: Bool
    , bValue :: Double }

data VarianceIndex = VarianceIndex {
      vFreq :: Double
    , vIdx  :: Double }


instance Storable Observation where
  sizeOf a = {#sizeof observation#}
  alignment a = alignment (undefined :: CDouble)
  peek p = Observation <$> liftM realToFrac ({#get observation->time #} p)
           <*> liftM realToFrac ({#get observation->value #} p)
  poke p x = do
    {#set observation.time #} p $ realToFrac $ oTime x
    {#set observation.value #} p $ realToFrac $ oValue x

cToBool 0 = False
cToBool 1 = True

boolToC False = 0
boolToC True = 1

instance Storable Bin where
  sizeOf a = {#sizeof bin#}
  alignment a = alignment (undefined :: CDouble)
  peek p = Bin <$> liftM cToBool ({#get bin->empty_flag#} p)
           <*> liftM realToFrac ({#get bin->value #} p)
  poke p x = do
    {#set bin.empty_flag #} p $ boolToC $ emptyFlag x
    {#set bin.value #} p $ realToFrac $ bValue x

instance Storable VarianceIndex where
  sizeOf a = {#sizeof variance_index#}
  alignment a = alignment (undefined :: CDouble)
  peek p = VarianceIndex <$> liftM realToFrac ({#get variance_index->freq#} p)
           <*> liftM realToFrac ({#get variance_index->v_idx #} p)
  poke p x = do
    {#set variance_index.freq #} p $ realToFrac $ vFreq x
    {#set variance_index.v_idx #} p $ realToFrac $ vIdx x


runLaflerKinman :: S.Vector Double -> S.Vector Observation -> S.Vector VarianceIndex
runLaflerKinman freqs obs = unsafePerformIO $ do
  bins <- SM.replicate (S.length obs + 1) $ Bin False 0
  vidx <- SM.new $ S.length freqs
  let (fptrBins, binL) = SM.unsafeToForeignPtr0 bins
  let (fptrVidx, vidxL) = SM.unsafeToForeignPtr0 vidx
  let (fptrFreqs, freqsL) = S.unsafeToForeignPtr0 freqs
  let (fptrObs, obsL) = S.unsafeToForeignPtr0 obs
  withForeignPtr fptrObs $ \ptrObs ->
    withForeignPtr fptrFreqs $ \ptrFreqs ->
       withForeignPtr fptrVidx $ \ptrVidx ->
           withForeignPtr fptrBins $ \ptrBins ->
               _runLaflerKinman ptrFreqs (fromIntegral freqsL) ptrObs (fromIntegral obsL) ptrBins 
                (fromIntegral binL) ptrVidx

  G.basicUnsafeFreeze vidx

foreign import ccall safe "RunLaflerKinman"
  _runLaflerKinman :: Ptr Double -> CInt -> Ptr Observation -> CInt -> Ptr Bin -> CInt -> Ptr VarianceIndex -> IO ()
