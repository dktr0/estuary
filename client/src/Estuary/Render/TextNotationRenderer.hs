module Estuary.Render.TextNotationRenderer where

import Data.Text
import Data.Time
import Data.Time.Clock.POSIX

import Estuary.Types.Context
import Estuary.Types.Tempo
import Estuary.Types.NoteEvent
import Estuary.Render.R
import qualified Sound.Tidal.Context as Tidal
import GHCJS.Types
import GHCJS.DOM.Types hiding (Text)


data TextNotationRenderer = TextNotationRenderer {
  parseZone :: Context -> Int -> Text -> UTCTime -> R (),
  scheduleTidalEvents :: Context -> Int -> R [(UTCTime,Tidal.ValueMap)],
  scheduleNoteEvents :: Context -> Int -> R [NoteEvent],
  scheduleWebDirtEvents :: Context -> Int -> R [JSVal], -- deprecated/temporary
  clearZone' :: Int -> R (),
  zoneAnimationFrame :: UTCTime -> Int -> R (),
  preAnimationFrame :: R (),
  postAnimationFrame :: R ()
}

emptyTextNotationRenderer :: TextNotationRenderer
emptyTextNotationRenderer = TextNotationRenderer {
  parseZone = \_ _ _ _ -> return (),
  scheduleTidalEvents = \_ _ -> return [],
  scheduleNoteEvents = \_ _ -> return [],
  clearZone' = \_ -> return (),
  zoneAnimationFrame = \_ _ -> return (),
  preAnimationFrame = return (),
  postAnimationFrame = return ()
  }


newtype ExoResult = ExoResult JSVal

instance PToJSVal ExoResult where pToJSVal (ExoResult x) = x

instance PFromJSVal ExoResult where pFromJSVal = ExoResult

foreign import javascript safe
  "$1.success"
  _exoResultSuccess :: ExoResult -> Bool

foreign import javascript safe
  "$1.error"
  _exoResultError :: ExoResult -> Text

exoResultToErrorText :: ExoResult -> Maybe Text
exoResultToErrorText x = case _exoResultSuccess x of
  True -> Nothing
  False -> Just $ _exoResultError x


utcTimeToWhenPOSIX :: UTCTime -> Double
utcTimeToWhenPOSIX = realToFrac . utcTimeToPOSIXSeconds
