{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.NoteEvent where

import Data.Time
import Data.Time.Clock.POSIX
import Data.Map as Map
import Data.Text as T
import Data.Text.Encoding
import Sound.Osc.Datum
import JavaScript.Object
import GHCJS.Types
import GHCJS.Marshal.Pure
import qualified Sound.Tidal.Context as Tidal
import Data.IntMap as IntMap
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Estuary.Resources
import Estuary.Types.Tempo
import Language.Javascript.JSaddle.Object
import Language.Javascript.JSaddle.Value
import Data.JSString.Text


type DatumMapEvent = (UTCTime, Map Text Datum)

type TidalEvent = (UTCTime, Tidal.ValueMap)

newtype NoteEvent = NoteEvent JSVal

instance PFromJSVal NoteEvent where pFromJSVal x = NoteEvent x

instance PToJSVal NoteEvent where pToJSVal (NoteEvent x) = x

datumToJSVal :: Datum -> JSVal
datumToJSVal (Int32 x) = pToJSVal x
datumToJSVal (Double x) = pToJSVal x
datumToJSVal (AsciiString x) = pToJSVal $ decodeUtf8 x
datumToJSVal _ = nullRef

valueToJSVal :: Tidal.Value -> JSVal
valueToJSVal (Tidal.VS x) = pToJSVal x
valueToJSVal (Tidal.VF x) = pToJSVal x
valueToJSVal (Tidal.VN x) = pToJSVal (Tidal.unNote x)
valueToJSVal (Tidal.VR x) = pToJSVal (realToFrac x :: Double)
valueToJSVal (Tidal.VI x) = pToJSVal x
valueToJSVal (Tidal.VB x) = pToJSVal (if x then pToJSVal (1::Int) else pToJSVal (0::Int))
valueToJSVal _ = nullRef
-- note: Tidal also has other constructors that we are currently ignoring

datumMapEventToNoteEvent :: MonadIO m => (UTCTime, Map Text Datum) -> m NoteEvent
datumMapEventToNoteEvent (whenUTC,m) = liftIO $ do
  o <- create
  Map.traverseWithKey (\k v -> unsafeSetProp (textToJSString k) (datumToJSVal v) o) m
  unsafeSetProp "whenPosix" (pToJSVal $ (realToFrac :: NominalDiffTime -> Double) $ utcTimeToPOSIXSeconds whenUTC) o
  j <- toJSVal o
  pure $ NoteEvent j

tidalEventToNoteEvent :: MonadIO m => (UTCTime,Tidal.ValueMap) -> m NoteEvent
tidalEventToNoteEvent (whenUTC,m) = liftIO $ do
  o <- create
  Map.traverseWithKey (\k v -> unsafeSetProp (textToJSString $ T.pack $ k) (valueToJSVal v) o) m
  unsafeSetProp "whenPosix" (pToJSVal $ (realToFrac :: NominalDiffTime -> Double) $ utcTimeToPOSIXSeconds whenUTC) o
  j <- toJSVal o
  pure $ NoteEvent j

-- if a provided NoteEvent has a when field then assume it is in POSIX units
-- put that in whenPosix instead and 
whenToPOSIXandAudio :: MonadIO m => (UTCTime, Double) -> NoteEvent -> m ()
whenToPOSIXandAudio clockDiff (NoteEvent j) = liftIO $ do
  o <- makeObject j
  props <- listProps o
  when (elem "when" props) $ do
    w <- unsafeGetProp "when" o
    unsafeSetProp "whenPosix" w o
    let whenUTC = posixSecondsToUTCTime $ realToFrac $ (pFromJSVal w :: Double)
    unsafeSetProp "whenAudio" (pToJSVal $ utcTimeToAudioSeconds clockDiff whenUTC) o
    unsafeSetProp "when" nullRef o

