module Main where

import Estuary.WebDirt.Stream
import Sound.Tidal.Context

main = do
  w <- webDirtStream
  w $ s (p "bd cp")
