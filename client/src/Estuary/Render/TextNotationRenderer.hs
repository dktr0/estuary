module Estuary.Render.TextNotationRenderer where

import Data.Text
import Data.Time

import Estuary.Types.Context
import Estuary.Render.R

data TextNotationRenderer = TextNotationRenderer {
  parseZone :: Context -> Int -> Text -> UTCTime -> R (),
  clearZone' :: Int -> R (),
  preAnimationFrame :: R (),
  zoneAnimationFrame :: UTCTime -> Int -> R (),
  postAnimationFrame :: R ()
}
