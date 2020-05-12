{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.NoteEvent (NoteEvent,mapToWebDirtMessage,controlMapToWebDirtMessage) where

import Data.Time
import Data.Map
import Data.Text
import Data.Text.Encoding
import Sound.OSC.Datum
import JavaScript.Object
import GHCJS.Types
import GHCJS.Marshal.Pure
import GHCJS.Marshal.Internal
import Data.String (fromString)
import Data.JSString.Text
import qualified Sound.Tidal.Context as Tidal


type NoteEvent = (UTCTime, Map Text Datum)


mapToWebDirtMessage :: (Double, Map Text Datum) -> IO JSVal
mapToWebDirtMessage (t,m) = do
  o <- create
  unsafeSetProp "when" (pToJSVal t) o
  traverseWithKey (\k v -> unsafeSetProp (textToJSString k) (datumToJSVal v) o) m
  return $ jsval o

datumToJSVal :: Datum -> JSVal
datumToJSVal (Int32 x) = pToJSVal x
datumToJSVal (Double x) = pToJSVal x
datumToJSVal (ASCII_String x) = pToJSVal $ decodeUtf8 x
datumToJSVal _ = nullRef


controlMapToWebDirtMessage :: (Double, Tidal.ControlMap) -> IO JSVal
controlMapToWebDirtMessage (t,m) = do
  o <- create
  unsafeSetProp "when" (pToJSVal t) o
  traverseWithKey (\k v -> unsafeSetProp (fromString k) (valueToJSVal v) o) m
  return $ jsval o

valueToJSVal :: Tidal.Value -> JSVal
valueToJSVal (Tidal.VI x) = pToJSVal x
valueToJSVal (Tidal.VF x) = pToJSVal x
valueToJSVal (Tidal.VS x) = pToJSVal x
valueToJSVal _ = nullRef
