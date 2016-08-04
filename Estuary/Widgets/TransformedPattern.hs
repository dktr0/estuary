module Estuary.Widgets.TransformedPattern where

import Reflex
import Reflex.Dom
import Estuary.Tidal.Types
import Estuary.Reflex.Utility
import Estuary.Widgets.PatternTransformer
import Estuary.Widgets.SoundPattern
import Control.Monad

trivialTransformedPattern :: MonadWidget t m => m (Dynamic t (TransformedPattern,()))
trivialTransformedPattern = el "div" $ do
  x <- trivialPatternTransformer
  y <- trivialSoundPattern
  z <- combineDyn (\(a,_) (b,_) -> TransformedPattern [a] b) x y
  display z
  mapDyn (\a -> (a,())) z
