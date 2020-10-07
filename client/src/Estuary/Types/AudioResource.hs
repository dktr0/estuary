{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.AudioResource where

import Data.IORef
import GHCJS.Types
import GHCJS.Marshal.Pure
import GHCJS.Foreign.Callback
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Sound.MusicW.AudioContext
import Control.Monad (when)

import Estuary.Types.AudioMeta
import Estuary.Types.Loadable

data AudioResource = AudioResource {
  audioMeta :: AudioMeta,
  audioLoadStatus :: IORef LoadStatus,
  audioJSVal :: JSVal
  }

instance Eq AudioResource where
  x == y = audioMeta x == audioMeta y

instance Show AudioResource where
  show = show . audioMeta

audioResourceFromMeta :: AudioMeta -> IO AudioResource
audioResourceFromMeta x = do
  y <- newIORef NotLoaded
  j <- newAudioJSVal
  return $ AudioResource {
    audioMeta = x,
    audioLoadStatus = y,
    audioJSVal = j
  }

foreign import javascript unsafe
  "{}"
  newAudioJSVal :: IO JSVal

instance Loadable AudioResource where

  loadStatus x = readIORef $ audioLoadStatus x

  load x = do
    s <- readIORef $ audioLoadStatus x
    when (s == NotLoaded) $ loadAudioResource x
    return $ audioJSVal x

loadAudioResource :: AudioResource -> IO ()
loadAudioResource x = do
  ls <- readIORef $ audioLoadStatus x
  case ls of
    NotLoaded -> do
      let url = audioURL $ audioMeta x
      writeIORef (audioLoadStatus x) Loading
      T.putStrLn $ "loading " <> url
      let url = audioURL $ audioMeta x
      r <- arraybufferXMLHttpRequest url
      cbLoad <- asyncCallback $ do
        cbSuccess <- asyncCallback1 $ \y -> do
          _setAudioBuffer (audioJSVal x) y
          writeIORef (audioLoadStatus x) Loaded
          T.putStrLn $ "loaded " <> url
        cbError <- asyncCallback1 $ \y -> do
          writeIORef (audioLoadStatus x) (LoadError "decoding error")
          T.putStrLn $ "error decoding " <> url
        ac <- getGlobalAudioContext
        decodeAudioData ac r cbSuccess cbError
        return ()
      onLoad r cbLoad
      cbError <- asyncCallback $ do
        let e = "error loading " <> url
        writeIORef (audioLoadStatus x) $ LoadError e
        T.putStrLn e
        return ()
      onError r cbError
      send r
    _ -> return ()

foreign import javascript unsafe
  "$1.buffer = $2;"
  _setAudioBuffer :: JSVal -> JSVal -> IO ()

newtype XMLHttpRequest = XMLHttpRequest JSVal

instance PFromJSVal XMLHttpRequest where pFromJSVal x = XMLHttpRequest x

instance PToJSVal XMLHttpRequest where pToJSVal (XMLHttpRequest x) = x

foreign import javascript safe
  "$r = new XMLHttpRequest(); $r.open('GET',$1,true); $r.responseType='arraybuffer';"
  arraybufferXMLHttpRequest :: Text -> IO XMLHttpRequest

foreign import javascript safe
  "$1.onload = $2;"
  onLoad :: XMLHttpRequest -> Callback (IO ()) -> IO ()

foreign import javascript safe
  "$1.onerror = $2;"
  onError :: XMLHttpRequest -> Callback (IO ()) -> IO ()

foreign import javascript safe
  "$1.send();"
  send :: XMLHttpRequest -> IO ()

foreign import javascript safe
  "$1.decodeAudioData($2.response,$3,$4)"
  decodeAudioData :: AudioContext -> XMLHttpRequest -> Callback (JSVal -> IO()) -> Callback (JSVal -> IO()) -> IO ()
