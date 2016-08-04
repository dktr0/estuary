module Estuary.Widgets.PatternTransformer where

import Reflex
import Reflex.Dom
import Estuary.Tidal.Types
import Estuary.Reflex.Utility

trivialPatternTransformer :: MonadWidget t m => m (Dynamic t (PatternTransformer,()))
trivialPatternTransformer = el "div $ do
  let ddMap = constDyn $ fromList [
    (NoTransformer," "),
    (Rev,"rev"),
    (Slow 2,"slow 2"),
    (Density 2,"density 2"),
    (Degrade,"degrade"),
    (DegradeBy 0.9,"degradeBy 0.9"),
    (Brak,"brak")]
  dd <- dropdown NoTransformer ddMap def
  mapDyn (\a -> (a,())) $ _dropdown_value dd
 >

  x <- button' "bd cp" $ SoundPattern (map simpleSound ["bd","cp"])
  y <- button' "arpy*4" $ SoundPattern (map simpleSound ["arpy","arpy","arpy","arpy"])
  z <- button' "~ arp" $ SoundPattern [silentSound,simpleSound "arp"]
  pattern <- holdDyn (SoundPattern []) $ leftmost [x,y,z]
  display pattern
  mapDyn (\a -> (a,())) pattern
