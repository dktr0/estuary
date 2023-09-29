{-# LANGUAGE JavaScriptFFI, OverloadedStrings, FlexibleContexts #-}

module Estuary.Render.WebDirt (
  WebDirt,
  newWebDirt,
  initializeWebAudio,
  performWebDirtHints,
  playSample,
  accessBufferForWebDirtEvent,
  setWebDirtAudioOutputs,
  voices) where

import Control.Monad
import Control.Monad.IO.Class
import Reflex.Dom
import Sound.MusicW
import Data.Text
import qualified Data.Text as T
import Data.Time
import Sound.Osc.Datum
import qualified Data.Map as Map
import Data.String (fromString)
import Data.JSString.Text
import GHCJS.Types
import GHCJS.Marshal.Pure
import Language.Javascript.JSaddle.Object
import Language.Javascript.JSaddle.Value
import Data.Text.Encoding


import Estuary.Types.Hint
import Estuary.Types.Tempo
import Estuary.Types.Location
import Estuary.Types.AudioMeta
import Estuary.Resources.AudioResource
import Estuary.Resources
import Estuary.Resources.Loadable
import Estuary.Types.NoteEvent
import qualified Sound.Tidal.Context as Tidal

newtype WebDirt = WebDirt JSVal

instance PToJSVal WebDirt where pToJSVal (WebDirt x) = x

instance PFromJSVal WebDirt where pFromJSVal = WebDirt

newWebDirt :: AudioIO m => Node -> m WebDirt
newWebDirt n = do
  ctx <- audioContext
  liftIO $ js_newWebDirt ctx n

foreign import javascript unsafe
  "$r = new WebDirt.WebDirt({ latency: 0, maxLateness: 0.010, audioContext: $1, destination: $2 });"
  js_newWebDirt :: AudioContext -> Node -> IO WebDirt

foreign import javascript unsafe
  "$1.initializeWebAudio()"
  initializeWebAudio :: WebDirt -> IO ()

foreign import javascript unsafe
  "try { $1.playSample($2) } catch(e) { console.log(e)} "
  playSample :: WebDirt -> JSVal -> IO ()

foreign import javascript unsafe
  "$1.audioOutputs = $2;"
  setWebDirtAudioOutputs :: WebDirt -> Int -> IO ()

performHint :: (PerformEvent t m, Reflex t, MonadIO (Performable m)) => WebDirt -> Event t Hint -> m ()
performHint wd ev = performEvent_ $ fmap (liftIO . (doHint wd)) ev

-- (PerformEvent t m, Reflex t)
performWebDirtHints :: (PerformEvent t m, Reflex t, MonadIO (Performable m)) => WebDirt -> Event t [Hint] -> m ()
performWebDirtHints wd evs = performEvent_ $ fmap (liftIO . (doHints wd)) evs

doHint :: WebDirt -> Hint -> IO ()
doHint wd (PreloadAudioBank x) = sampleHint wd (pToJSVal x)
doHint _ _ = return ()

doHints :: WebDirt -> [Hint] -> IO ()
doHints wd = mapM_ (doHint wd)

foreign import javascript unsafe
  "$1.sampleHint($2)"
  sampleHint :: WebDirt -> JSVal -> IO ()

foreign import javascript unsafe
  "$1.voices"
  voices :: WebDirt -> IO Int

{-
mapTextJSValToJSVal :: (Double, Map.Map Text JSVal) -> IO JSVal
mapTextJSValToJSVal (t,m) = do
  o <- create
  unsafeSetProp "when" (pToJSVal t) o
  Map.traverseWithKey (\k v -> unsafeSetProp (textToJSString k) v o) m
  return $ jsval o
-}

-- given a JSVal that contains a JavaScript object containing parameters ready for consumption
-- by WebDirt's playSample method, use the audio resources system to substitute a buffer
-- this is probably just a temporary hack while we are (temporarily) allowing a "direct to WebDirt"
-- pathway in connection with some languages.

accessBufferForWebDirtEvent :: MonadIO m => Resources -> NoteEvent -> m ()
accessBufferForWebDirtEvent r (NoteEvent j) = do
  o <- liftIO $ makeObject j
  props <- liftIO $ listProps o
  when (elem "s" props) $ do
    s <- pFromJSVal <$> (liftIO $ unsafeGetProp "s" o)
    n <- case elem "n" props of
      False -> pure 0
      True -> pFromJSVal <$> (liftIO $ unsafeGetProp "n" o)
    x <- accessAudioResource r (s,n)
    liftIO $ case x of
      Left err -> putStrLn $ "accessBufferForWebDirtEvent error: " ++ show err
      Right ar -> unsafeSetProp "buffer" (audioJSVal ar) o

makeNoteEventSafe :: MonadIO m => NoteEvent -> m ()
makeNoteEventSafe (NoteEvent j) = liftIO $ do
  o <- makeObject j
  unsafeSetProp "crush" nullRef o
  unsafeSetProp "coarse" nullRef o
  unsafeSetProp "shape" nullRef o

datumsToLocation :: Maybe Datum -> Maybe Datum -> Maybe Location
datumsToLocation (Just (AsciiString x)) Nothing = Just (decodeUtf8 x,0)
datumsToLocation (Just (AsciiString x)) (Just (Int32 y)) = Just (decodeUtf8 x,fromIntegral y)
datumsToLocation (Just (AsciiString x)) (Just (Double y)) = Just (decodeUtf8 x,floor y)
datumsToLocation _ _ = Nothing
