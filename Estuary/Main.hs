module Main where

import Reflex
import Reflex.Dom
import Data.FileEmbed (embedFile)

import Estuary.WebDirt.Stream
import Estuary.Frame

main :: IO ()
main = do
  stream <- webDirtStream
  mainWidgetWithCss $ (Data.FileEmbed.embedFile "../static/css/helperwidgets.css") $ frame stream trivialEditor

trivialEditor :: MonadWidget t m => m (Dynamic t (SoundPattern,()))
trivialEditor = do
  x <- button ""
  y <- button ""
  z <- button ""
  pattern <- holdDyn (SoundPattern []) $ leftmost [x,y,z]
  mapDyn (\a -> (a,())) pattern
