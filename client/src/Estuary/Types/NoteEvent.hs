{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.NoteEvent where

import Data.Time
import Data.Map
import Data.Text
import Data.Text.Encoding
import Sound.OSC.Datum
import JavaScript.Object
import GHCJS.Types
import GHCJS.Marshal.Pure
import qualified Sound.Tidal.Context as Tidal


type NoteEvent = (UTCTime, Map Text Datum)

datumToJSVal :: Datum -> JSVal
datumToJSVal (Int32 x) = pToJSVal x
datumToJSVal (Double x) = pToJSVal x
datumToJSVal (ASCII_String x) = pToJSVal $ decodeUtf8 x
datumToJSVal _ = nullRef

valueToJSVal :: Tidal.Value -> JSVal
valueToJSVal (Tidal.VI x _) = pToJSVal x
valueToJSVal (Tidal.VF x _) = pToJSVal x
valueToJSVal (Tidal.VS x _) = pToJSVal x
valueToJSVal _ = nullRef
