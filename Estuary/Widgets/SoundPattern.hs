module Estuary.Widgets.SoundPattern where

import Reflex
import Reflex.Dom
import Estuary.Tidal.Types
import Estuary.Reflex.Utility

trivialSoundPattern :: MonadWidget t m => m (Dynamic t (SoundPattern,()))
trivialSoundPattern = el "div" $ do
  x <- button' "bd cp" $ SoundPattern (map simpleSound ["bd","cp"])
  y <- button' "arpy*4" $ SoundPattern (map simpleSound ["arpy","arpy","arpy","arpy"])
  z <- button' "~ arp" $ SoundPattern [silentSound,simpleSound "arp"]
  pattern <- holdDyn (SoundPattern []) $ leftmost [x,y,z]
  display pattern
  mapDyn (\a -> (a,())) pattern
