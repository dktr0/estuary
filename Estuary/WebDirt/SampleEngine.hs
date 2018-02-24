module Estuary.WebDirt.SampleEngine where

import Sound.Tidal.Context

class SampleEngine e where
  getClockDiff :: e -> IO Double -- diff btw clock used to play sample events and POSIX
  playSample :: e -> (Double,ParamMap) -> IO ()
  getLevels :: e -> IO [Double]
