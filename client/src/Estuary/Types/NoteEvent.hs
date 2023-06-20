{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.NoteEvent where

import Data.Time
import Data.Map
import Data.Text
import Data.Text.Encoding
import Sound.Osc.Datum
import JavaScript.Object
import GHCJS.Types
import GHCJS.Marshal.Pure
import qualified Sound.Tidal.Context as Tidal
import Data.IntMap as IntMap

type NoteEvent = (UTCTime, Map Text Datum)

datumToJSVal :: Datum -> JSVal
datumToJSVal (Int32 x) = pToJSVal x
datumToJSVal (Double x) = pToJSVal x
datumToJSVal (AsciiString x) = pToJSVal $ decodeUtf8 x
datumToJSVal _ = nullRef

datumToInteger :: Datum -> Maybe Integer
datumToInteger (Int32 x) = Just $ toInteger x
datumToInteger _ = Nothing

valueToJSVal :: Tidal.Value -> JSVal
valueToJSVal (Tidal.VS x) = pToJSVal x
valueToJSVal (Tidal.VF x) = pToJSVal x
valueToJSVal (Tidal.VN x) = pToJSVal (Tidal.unNote x)
valueToJSVal (Tidal.VR x) = pToJSVal (realToFrac x :: Double)
valueToJSVal (Tidal.VI x) = pToJSVal x
valueToJSVal (Tidal.VB x) = pToJSVal (if x then pToJSVal (1::Int) else pToJSVal (0::Int))
-- note: Tidal also has other constructors that we are currently ignoring
valueToJSVal _ = nullRef

valueToDatum :: Tidal.Value -> Datum
valueToDatum (Tidal.VS x) = AsciiString $ encodeUtf8 $ pack x
valueToDatum (Tidal.VF x) = Double x
valueToDatum (Tidal.VN x) = Double (Tidal.unNote x)
valueToDatum (Tidal.VR x) = Double (realToFrac x)
valueToDatum (Tidal.VI x) = Int32 $ fromInteger $ toInteger x
valueToDatum (Tidal.VB x) = Int32 (if x then 1 else 0)
-- note: Tidal also has other constructors that we are currently ignoring
valueToDatum _ = Int32 0 -- unmatched cases just become the number 0

tidalEventToNoteEvent :: (UTCTime,Tidal.ValueMap) -> NoteEvent
tidalEventToNoteEvent (utc,m) = (utc,fmap valueToDatum  $ Data.Map.mapKeys pack m )
