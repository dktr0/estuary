module Estuary.Render.TextNotationRenderer where

import Data.Text
import Data.Time

import Estuary.Types.Context
import Estuary.Types.Tempo
import Estuary.Render.R

data TextNotationRenderer = TextNotationRenderer {
  parseZone :: Context -> Int -> Text -> UTCTime -> R (),
  clearZone' :: Int -> R (),
  zoneAnimationFrame :: UTCTime -> Int -> R (),
  preAnimationFrame :: R (),
  postAnimationFrame :: R ()
}
