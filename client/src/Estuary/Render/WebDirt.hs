{-# LANGUAGE JavaScriptFFI #-}

module Estuary.Render.WebDirt (WebDirt, newWebDirt, initializeWebAudio, performHints, playSample) where

import GHCJS.Types
import GHCJS.Marshal.Pure
import Control.Monad.IO.Class (liftIO)
import Reflex.Dom

import Estuary.Types.Hint
import Sound.MusicW

newtype WebDirt = WebDirt JSVal

instance PToJSVal WebDirt where pToJSVal (WebDirt x) = x

instance PFromJSVal WebDirt where pFromJSVal = WebDirt

newWebDirt :: AudioIO m => Node -> m WebDirt
newWebDirt n = do
  ctx <- audioContext
  liftIO $ js_newWebDirt ctx n

foreign import javascript unsafe
  "$r = new WebDirt('samples/sampleMap.json','samples',0,null,0.010,$1,$2)"
  js_newWebDirt :: AudioContext -> Node -> IO WebDirt
  -- 0 is additional delay/latency added to all events sent to WebDirt
  -- 0.010 is maximum lateness after which WebDirt silently drops sample events
  -- JSVal is web audio node provided as a sink/destination for all synths

foreign import javascript unsafe
  "$1.initializeWebAudio()"
  initializeWebAudio :: WebDirt -> IO ()

foreign import javascript unsafe
  "try { $1.playSample($2) } catch(e) { console.log(e)} "
  playSample :: WebDirt -> JSVal -> IO ()

performHint :: MonadWidget t m => WebDirt -> Event t Hint -> m ()
performHint wd ev = performEvent_ $ fmap (liftIO . (doHint wd)) ev

performHints :: MonadWidget t m => WebDirt -> Event t [Hint] -> m ()
performHints wd evs = performEvent_ $ fmap (liftIO . (doHints wd)) evs

doHint :: WebDirt -> Hint -> IO ()
doHint wd (SampleHint x) = sampleHint wd (pToJSVal x)
doHint _ _ = return ()

doHints :: WebDirt -> [Hint] -> IO ()
doHints wd = mapM_ (doHint wd)

foreign import javascript unsafe
  "$1.sampleHint($2)"
  sampleHint :: WebDirt -> JSVal -> IO ()
