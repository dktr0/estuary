{-# LANGUAGE JavaScriptFFI #-}

module Estuary.WebDirt.WebDirt (WebDirt, newWebDirt, initializeWebAudio,performHint) where

import GHCJS.Types
import GHCJS.Marshal.Pure
import Control.Monad.IO.Class (liftIO)
import Reflex.Dom
import qualified Sound.Tidal.Context as Tidal

import qualified Estuary.WebDirt.SampleEngine as S
import Estuary.WebDirt.Foreign (createObjFromMap)
import Estuary.Types.Hint
import Estuary.Render.AudioContext

newtype WebDirt = WebDirt JSVal

instance PToJSVal WebDirt where pToJSVal (WebDirt x) = x

instance PFromJSVal WebDirt where pFromJSVal = WebDirt

foreign import javascript unsafe
  "$r = new WebDirt('WebDirt/sampleMap.json','Dirt/samples',null,null,null,$1)"
  newWebDirt :: AudioContext -> IO WebDirt

foreign import javascript unsafe
  "$1.initializeWebAudio()"
  initializeWebAudio :: WebDirt -> IO ()

instance S.SampleEngine WebDirt where
  getClockDiff wd = getClockDiff wd
  playSample wd x = playSample wd x
  getPeakLevels wd = peakLevels wd
  getRmsLevels wd = rmsLevels wd

foreign import javascript unsafe
  "try { $r = $1.getCurrentTime() } catch(e) { console.log(e)} "
  getCurrentTime :: WebDirt -> IO Double

foreign import javascript unsafe
  "try { $r = $1.clockDiff; } catch(e) { console.log(e)}"
  getClockDiff :: WebDirt -> IO Double


playSample :: WebDirt -> (Double,Tidal.ControlMap) -> IO ()
playSample wd (t,e) = do
  object <- createObjFromMap t e
  playSample_ wd object

foreign import javascript unsafe
  "try { $1.playSample($2) } catch(e) { console.log(e)} "
  playSample_ :: WebDirt -> JSVal -> IO ()


performHint :: MonadWidget t m => WebDirt -> Event t Hint -> m ()
performHint wd ev = performEvent_ $ fmap (liftIO . (doHint wd)) ev

doHint :: WebDirt -> Hint -> IO ()
doHint wd (SampleHint x) = sampleHint wd (pToJSVal x)
doHint _ _ = return ()

foreign import javascript unsafe
  "$1.sampleHint($2)"
  sampleHint :: WebDirt -> JSVal -> IO ()


peakLevels :: WebDirt -> IO [Double]
peakLevels wd = do
  l <- peakLevelLeft wd
  r <- peakLevelRight wd
  return [l,r]

rmsLevels :: WebDirt -> IO [Double]
rmsLevels wd = do
  l <- rmsLevelLeft wd
  r <- rmsLevelRight wd
  return [l,r]

foreign import javascript unsafe
  "$r = $1.levelMeter.peak[0]"
  peakLevelLeft :: WebDirt -> IO Double

foreign import javascript unsafe
  "$r = $1.levelMeter.peak[1]"
  peakLevelRight :: WebDirt -> IO Double

foreign import javascript unsafe
  "$r = $1.levelMeter.rms[0]"
  rmsLevelLeft :: WebDirt -> IO Double

foreign import javascript unsafe
  "$r = $1.levelMeter.rms[1]"
  rmsLevelRight :: WebDirt -> IO Double
