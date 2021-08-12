{-# LANGUAGE OverloadedStrings #-}

module Estuary.Resources.AudioResource where

-- An AudioResource is a type representing an audio file that is asynchronously loaded given some URL.
-- (ie. it is an instance of Loadable from Estuary.Resources.Loadable)


import Data.IORef
import GHCJS.Types
import GHCJS.Marshal.Pure
import GHCJS.Foreign.Callback
import Language.Javascript.JSaddle.Value
import Language.Javascript.JSaddle.Object
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Sound.MusicW.AudioContext
import Control.Monad (when)

import Estuary.Types.AudioMeta
import Estuary.Resources.Loadable
import Estuary.Resources.XMLHttpRequest

data AudioResource = AudioResource {
  audioMeta :: AudioMeta,
  audioLoadStatus :: IORef LoadStatus,
  audioJSVal :: JSVal
  }

instance PToJSVal AudioResource where pToJSVal = audioJSVal

instance Eq AudioResource where
  x == y = audioMeta x == audioMeta y

instance Show AudioResource where
  show = show . audioMeta

instance Loadable AudioResource where
  loadStatus x = readIORef $ audioLoadStatus x
  newLoadable url _ = do
    x <- audioResourceFromMeta (AudioMeta url 0)
    loadAudioResource x
    return x

audioResourceFromMeta :: AudioMeta -> IO AudioResource
audioResourceFromMeta x = do
  y <- newIORef NotLoaded
  j <- obj >>= toJSVal
  return $ AudioResource {
    audioMeta = x,
    audioLoadStatus = y,
    audioJSVal = j
  }

loadAudioResource :: AudioResource -> IO ()
loadAudioResource x = do
  ls <- readIORef $ audioLoadStatus x
  case ls of
    NotLoaded -> do
      let url = audioURL $ audioMeta x
      writeIORef (audioLoadStatus x) Loading
      T.putStrLn $ "loading AudioResource " <> url
      let url = audioURL $ audioMeta x
      r <- arraybufferXMLHttpRequest url
      onLoad r $ \_ -> do
        cbSuccess <- asyncCallback1 $ \y -> do
          _setAudioBuffer (audioJSVal x) y
          writeIORef (audioLoadStatus x) Loaded
          T.putStrLn $ "loaded AudioResource " <> url
        cbError <- asyncCallback1 $ \y -> do
          writeIORef (audioLoadStatus x) (LoadError "decoding error")
          T.putStrLn $ "error decoding AudioResource " <> url
        ac <- getGlobalAudioContext
        decodeAudioData ac r cbSuccess cbError
        return ()
      onError r $ do
        let e = "error loading AudioResource " <> url
        writeIORef (audioLoadStatus x) $ LoadError e
        T.putStrLn e
        return ()
      send r
    _ -> return ()

foreign import javascript safe
  "$1.decodeAudioData($2.response,$3,$4)"
  decodeAudioData :: AudioContext -> XMLHttpRequest -> Callback (JSVal -> IO()) -> Callback (JSVal -> IO()) -> IO ()

foreign import javascript unsafe
  "$1.buffer = $2;"
  _setAudioBuffer :: JSVal -> JSVal -> IO ()
