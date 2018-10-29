{-# LANGUAGE JavaScriptFFI #-}

module Estuary.WebDirt.WebDirt (WebDirt, newWebDirt, performHint) where

import qualified GHCJS.Types as T
import Control.Monad.IO.Class (liftIO)
import qualified GHCJS.Marshal.Pure as P
import qualified Sound.Tidal.Context as Tidal
import Reflex.Dom

import qualified Estuary.WebDirt.SampleEngine as S
import Estuary.WebDirt.Foreign (createObjFromMap)
import Estuary.Types.Hint

newtype WebDirt = WebDirt T.JSVal

instance S.SampleEngine WebDirt where
  getClockDiff wd = getClockDiff wd
  playSample wd x = playSample wd x
  getPeakLevels wd = peakLevels wd
  getRmsLevels wd = rmsLevels wd

newWebDirt :: IO WebDirt
newWebDirt = webDirt_ >>= return . WebDirt

getCurrentTime :: WebDirt -> IO Double
getCurrentTime (WebDirt j) = getCurrentTime_ j

getClockDiff :: WebDirt -> IO Double
getClockDiff (WebDirt j) = getClockDiff_ j

playSample :: WebDirt -> (Double,Tidal.ParamMap) -> IO ()
playSample (WebDirt j) (t,e) = do
  object <- createObjFromMap t e
  playSample_ j object

doHint :: WebDirt -> Hint -> IO ()
doHint (WebDirt j) (SampleHint x) = sampleHint_ j (P.pToJSVal x)
doHint _ _ = return ()


performHint :: MonadWidget t m => WebDirt -> Event t Hint -> m ()
performHint wd ev = performEvent_ $ fmap (liftIO . (doHint wd)) ev

peakLevels :: WebDirt -> IO [Double]
peakLevels (WebDirt j) = do
  l <- peakLevelLeft_ j
  r <- peakLevelRight_ j
  return [l,r]

rmsLevels :: WebDirt -> IO [Double]
rmsLevels (WebDirt j) = do
  l <- rmsLevelLeft_ j
  r <- rmsLevelRight_ j
  return [l,r]


-- FFI definitions below this line:

foreign import javascript unsafe
  "$r = ___globalWebDirt"
  webDirt_ :: IO T.JSVal

foreign import javascript unsafe
  "try { $r = $1.getCurrentTime() } catch(e) { console.log(e)} "
  getCurrentTime_ :: T.JSVal -> IO Double

foreign import javascript unsafe
  "try { $r = $1.clockDiff; } catch(e) { console.log(e)}"
  getClockDiff_ :: T.JSVal -> IO Double

foreign import javascript unsafe
  "try { ___globalWebDirt.playSample($2)} catch(e) { console.log(e)} "
  playSample_ :: T.JSVal -> T.JSVal -> IO ()

foreign import javascript safe
  "$1.sampleHint($2)"
  sampleHint_ :: T.JSVal -> T.JSVal -> IO ()

foreign import javascript unsafe
  "$r = $1.levelMeter.peak[0]"
  peakLevelLeft_ :: T.JSVal -> IO Double

foreign import javascript unsafe
  "$r = $1.levelMeter.peak[1]"
  peakLevelRight_ :: T.JSVal -> IO Double

foreign import javascript unsafe
  "$r = $1.levelMeter.rms[0]"
  rmsLevelLeft_ :: T.JSVal -> IO Double

foreign import javascript unsafe
  "$r = $1.levelMeter.rms[1]"
  rmsLevelRight_ :: T.JSVal -> IO Double
