{-# LANGUAGE JavaScriptFFI #-}

module Estuary.WebDirt.WebDirt (WebDirt, webDirt, performHint) where

import qualified GHCJS.Types as T
import Control.Monad.IO.Class (liftIO)
import qualified GHCJS.Marshal.Pure as P
import qualified Sound.Tidal.Context as Tidal
import Reflex.Dom

import qualified Estuary.WebDirt.SampleEngine as S
import Estuary.WebDirt.Foreign (createObjFromMap)
import Estuary.Types.Hint

data WebDirt = WebDirt T.JSVal

instance S.SampleEngine WebDirt where
  getClockDiff wd = getClockDiff wd
  playSample wd x = playSample wd x
  getLevels wd = return [] -- getLevels wd

webDirt :: IO WebDirt
webDirt = webDirt_ >>= return . WebDirt

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
doHint _ (TempoHint _) = return ()

performHint :: MonadWidget t m => WebDirt -> Event t Hint -> m ()
performHint wd ev = performEvent_ $ fmap (liftIO . (doHint wd)) ev


-- FFI definitions below this line:

foreign import javascript unsafe
  "$r = ___globalWebDirt"
  webDirt_ :: IO T.JSVal

foreign import javascript unsafe
  "try { $r = $1.getCurrentTime() } catch(e) { console.log(e)} "
  getCurrentTime_ :: T.JSVal -> IO Double

foreign import javascript unsafe
  "try { $r = $1.clockDiff } catch(e) { console.log(e)}"
  getClockDiff_ :: T.JSVal -> IO Double

foreign import javascript unsafe
  "try { ___globalWebDirt.playSample($2)} catch(e) { console.log(e)} "
  playSample_ :: T.JSVal -> T.JSVal -> IO ()

foreign import javascript safe
  "$1.sampleHint($2)"
  sampleHint_ :: T.JSVal -> T.JSVal -> IO ()
