module Main where

import Reflex
import Reflex.Dom
import Control.Monad (liftM)
import Sound.Tidal.Context
import Estuary.WebDirt.Stream
import Estuary.Page
import Estuary.Widgets.SoundPattern
import Estuary.Widgets.TransformedPattern

main :: IO ()
main = do
  webDirt <- webDirtStream
  mainWidget $ multipage webDirt pages

-- pages :: MonadWidget t m => [m (Dynamic t ParamPattern)]
pages = [
  ("trivialSoundPattern",widgetToPage trivialSoundPattern),
  ("trivialTransformedPattern",widgetToPage trivialTransformedPattern)
  ]
