{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}

module Estuary.Render.WebDirt (WebDirt, newWebDirt, initializeWebAudio, performHints, playSample, audioResourceTest, mapTextJSValToJSVal, mapStringJSValToJSVal, noteEventToWebDirtJSVal, tidalEventToWebDirtJSVal) where

import GHCJS.Types
import GHCJS.Marshal.Pure
import Control.Monad.IO.Class (liftIO)
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
import JavaScript.Object
import Data.Text.Encoding


import Estuary.Types.Hint
import Estuary.Types.Tempo
import Estuary.Types.AudioMeta
import Estuary.Types.AudioResource
import Estuary.Types.ResourceMap
import Estuary.Types.Loadable
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

-- temporary, just for testing
foreign import javascript unsafe
  "try { $1.playSample({ buffer: $2 }) } catch(e) { console.log(e)} "
  playBuffer :: WebDirt -> JSVal -> IO ()


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


noteEventToWebDirtJSVal :: AudioMap -> (UTCTime,Double) -> NoteEvent -> IO (Maybe JSVal)
noteEventToWebDirtJSVal aMap cDiff (utc,m) = do
  let s = Map.lookup "s" m
  let n = Map.lookup "n" m
  case datumsToLocation s n of
    Nothing -> return Nothing
    Just loc -> do
      res <- access loc aMap
      case res of
        Right res' -> do
          let t' = utcTimeToAudioSeconds cDiff utc
          let m' = Map.insert "buffer" res' $ fmap datumToJSVal m -- :: Map Text JSVal
          Just <$> mapTextJSValToJSVal (t',m')
        Left _ -> return Nothing

tidalEventToWebDirtJSVal :: AudioMap -> (UTCTime,Double) -> (UTCTime, Tidal.ControlMap) -> IO (Maybe JSVal)
tidalEventToWebDirtJSVal aMap cDiff (utc,m) = do
  let s = Map.lookup "s" m
  let n = Map.lookup "n" m
  case valuesToLocation s n of
    Nothing -> return Nothing
    Just loc -> do
      res <- access loc aMap
      case res of
        Right res' -> do
          let t' = utcTimeToAudioSeconds cDiff utc
          let m' = Map.insert "buffer" res' $ fmap valueToJSVal m -- :: Map Text JSVal
          Just <$> mapStringJSValToJSVal (t',m')
        Left _ -> return Nothing

mapTextJSValToJSVal :: (Double, Map.Map Text JSVal) -> IO JSVal
mapTextJSValToJSVal (t,m) = do
  o <- create
  unsafeSetProp "when" (pToJSVal t) o
  Map.traverseWithKey (\k v -> unsafeSetProp (textToJSString k) v o) m
  return $ jsval o

-- for Tidal-sourced events, which arrive here as Map String JSVal
mapStringJSValToJSVal :: (Double, Map.Map String JSVal) -> IO JSVal
mapStringJSValToJSVal (t,m) = do
  o <- create
  unsafeSetProp "when" (pToJSVal t) o
  Map.traverseWithKey (\k v -> unsafeSetProp (fromString k) v o) m
  return $ jsval o

datumsToLocation :: Maybe Datum -> Maybe Datum -> Maybe Location
datumsToLocation (Just (ASCII_String x)) Nothing = Just (decodeUtf8 x,0)
datumsToLocation (Just (ASCII_String x)) (Just (Int32 y)) = Just (decodeUtf8 x,fromIntegral y)
datumsToLocation _ _ = Nothing

valuesToLocation :: Maybe Tidal.Value -> Maybe Tidal.Value -> Maybe Location
valuesToLocation (Just (Tidal.VS x)) Nothing = Just (T.pack x,0)
valuesToLocation (Just (Tidal.VS x)) (Just (Tidal.VF y)) = Just (T.pack x,floor y)
valuesToLocation (Just (Tidal.VS x)) (Just (Tidal.VI y)) = Just (T.pack x,y)
valuesToLocation _ _ = Nothing

audioResourceTest :: MonadWidget t m => WebDirt -> m ()
audioResourceTest wd = do
  let url = "samples/cp/HANDCLP0.wav"
  ar <- liftIO $ audioResourceFromMeta $ AudioMeta url 0.0
  w <- button "preload"
  performEvent_ $ ffor w (const $ liftIO $ (load ar >> return ()))
  x <- button "play"
  performEvent_ $ ffor x (const $ liftIO $ do
    y <- load ar
    case y of
      Right j -> do
        putStrLn "playing buffer"
        playBuffer wd j
      Left e -> putStrLn $ "audioResourceTest can't play because status is " ++ show e
    )
