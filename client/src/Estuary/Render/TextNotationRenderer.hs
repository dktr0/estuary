module Estuary.Render.TextNotationRenderer where

import Data.Text
import Data.Time

import Estuary.Types.Context
import Estuary.Types.Tempo
import Estuary.Types.NoteEvent
import Estuary.Render.R
import qualified Sound.Tidal.Context as Tidal

data TextNotationRenderer = TextNotationRenderer {
  parseZone :: Context -> Int -> Text -> UTCTime -> R (),
  scheduleTidalEvents :: Context -> Int -> R [(UTCTime,Tidal.ValueMap)],
  scheduleNoteEvents :: Context -> Int -> R [NoteEvent],
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
