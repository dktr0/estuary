module Main where

import Reflex
import Reflex.Dom
import Estuary.WebDirt.Stream
import Estuary.Frame
import Estuary.Tidal.Types
import Estuary.Reflex.Utility
import Estuary.Widgets.Sound
import Control.Monad (liftM)

main :: IO ()
main = do
  stream <- webDirtStream
  mainWidget $ frame stream trivialEditor'

trivialEditor :: MonadWidget t m => m (Dynamic t (SoundPattern,()))
trivialEditor = do
  x <- button' "bd cp" $ SoundPattern (map simpleSound ["bd","cp"])
  y <- button' "arpy*4" $ SoundPattern (map simpleSound ["arpy","arpy","arpy","arpy"])
  z <- button' "~ arp" $ SoundPattern [silentSound,simpleSound "arp"]
  pattern <- holdDyn (SoundPattern []) $ leftmost [x,y,z]
  mapDyn (\a -> (a,())) pattern

trivialEditor' :: MonadWidget t m => m (Dynamic t (SoundPattern,Event t ChildSignal))
trivialEditor' = do
  a <- textWidget (Sound Nothing) never-- Dynamic t (sound, Event childsig)
  forDyn a (\(sound,event)->(SoundPattern [sound],event))
