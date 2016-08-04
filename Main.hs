module Main where

import Reflex
import Reflex.Dom
import Estuary.WebDirt.Stream
import Estuary.Frame
import Estuary.Tidal.Types
import Estuary.Reflex.Utility
import Control.Monad (liftM)

import Estuary.Widgets.TransformedPattern

main :: IO ()
main = do
  stream <- webDirtStream
  mainWidget $ frame stream trivialTransformedPattern
