{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}

module Estuary.Render.WebDirt (
  WebDirt,
  newWebDirt,
  initializeWebAudio,
  performHints,
  playSample,
  mapTextJSValToJSVal,
  noteEventToWebDirtJSVal,
  accessBufferForWebDirtEvent,
  setWebDirtAudioOutputs) where

import Control.Monad
import Control.Monad.IO.Class
import Reflex.Dom
import Sound.MusicW
import Data.Text
import qualified Data.Text as T
import Data.Time
import Sound.OSC.Datum
import qualified Data.Map as Map
import Data.String (fromString)
import Data.JSString.Text
import GHCJS.Types
import GHCJS.Marshal.Pure
import Language.Javascript.JSaddle.Object
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

makeNoteEventSafe :: Map.Map Text Datum -> Map.Map Text Datum
makeNoteEventSafe = Map.delete "crush" . Map.delete "coarse" . Map.delete "shape"

noteEventToWebDirtJSVal :: Bool -> Resources -> (UTCTime,Double) -> NoteEvent -> IO (Maybe JSVal)
noteEventToWebDirtJSVal unsafe r cDiff (utc,m) = do
  let mSafe = if unsafe then m else makeNoteEventSafe m
  let s = Map.lookup "s" mSafe
  let n = Map.lookup "n" mSafe
  case datumsToLocation s n of
    Nothing -> return Nothing
    Just loc -> do
      res <- accessAudioResource r loc
      case res of
        Right res' -> do
          let t' = utcTimeToAudioSeconds cDiff utc
          let m' = Map.insert "buffer" (pToJSVal res') $ fmap datumToJSVal mSafe -- :: Map Text JSVal
          Just <$> mapTextJSValToJSVal (t',m')
        Left _ -> return Nothing

makeTidalEventSafe :: Tidal.ValueMap -> Tidal.ValueMap
makeTidalEventSafe = Map.delete "crush" . Map.delete "coarse" . Map.delete "shape"


mapTextJSValToJSVal :: (Double, Map.Map Text JSVal) -> IO JSVal
mapTextJSValToJSVal (t,m) = do
  o <- create
  unsafeSetProp "when" (pToJSVal t) o
  Map.traverseWithKey (\k v -> unsafeSetProp (textToJSString k) v o) m
  return $ jsval o


-- given a JSVal that contains a JavaScript object containing parameters ready for consumption
-- by WebDirt's playSample method, use the audio resources system to substitute a buffer
-- this is probably just a temporary hack while we are (temporarily) allowing a "direct to WebDirt"
-- pathway in connection with some languages.

accessBufferForWebDirtEvent :: MonadIO m => Resources -> JSVal -> m JSVal
accessBufferForWebDirtEvent r j = do
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
  pure j

datumsToLocation :: Maybe Datum -> Maybe Datum -> Maybe Location
datumsToLocation (Just (ASCII_String x)) Nothing = Just (decodeUtf8 x,0)
datumsToLocation (Just (ASCII_String x)) (Just (Int32 y)) = Just (decodeUtf8 x,fromIntegral y)
datumsToLocation _ _ = Nothing
