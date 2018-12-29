{-# LANGUAGE JavaScriptFFI #-}
module Estuary.Render.AudioContext where

import GHCJS.Types
import GHCJS.Marshal.Pure
import Data.Time

newtype AudioContext = AudioContext { audioContextJSVal :: JSVal }

instance PToJSVal AudioContext where pToJSVal (AudioContext x) = x

instance PFromJSVal AudioContext where pFromJSVal = AudioContext

foreign import javascript safe
  "if (window.___ac == null) { \
  \    window.___ac = new (window.AudioContext || window.webkitAudioContext)();\
  \} $r = window.___ac;"
  getAudioContext :: IO AudioContext

foreign import javascript safe
  "$1.currentTime"
  getAudioTimeDouble :: AudioContext -> IO Double

getAudioTime :: AudioContext -> IO UTCTime
getAudioTime ctx = do
  x <- getAudioTimeDouble ctx
  return $ UTCTime {
    utctDay = toEnum 0,
    utctDayTime = realToFrac x -- *** this might not work if audio context runs for more than one day...
  }

audioClockDiff :: AudioContext -> IO NominalDiffTime
audioClockDiff ctx = diffUTCTime <$> getCurrentTime <*> getAudioTime ctx
