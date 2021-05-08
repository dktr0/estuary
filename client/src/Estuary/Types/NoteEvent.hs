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
valueToJSVal (Tidal.VS x) = pToJSVal x
valueToJSVal (Tidal.VF x) = pToJSVal x
valueToJSVal (Tidal.VN x) = pToJSVal (Tidal.unNote x)
valueToJSVal (Tidal.VR x) = pToJSVal (realToFrac x :: Double)
valueToJSVal (Tidal.VI x) = pToJSVal x
valueToJSVal (Tidal.VB x) = pToJSVal (if x then pToJSVal (1::Int) else pToJSVal (0::Int))
-- Tidal also has constructor Tidal.VX [Word8] _ for OSC blobs, but that has no obvious application yet in Estuary
valueToJSVal _ = nullRef
