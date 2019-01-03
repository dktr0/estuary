{-# LANGUAGE JavaScriptFFI #-}
module Estuary.Render.AudioContext (module Sound.MusicW.AudioRoutingGraph, module Estuary.Render.AudioContext) where

import GHCJS.Types
import GHCJS.Marshal.Pure
import Data.Time
import Sound.MusicW.AudioRoutingGraph

-- | This 'utility' module is for definitions (probably only ever a few) that extend the
-- interface to the Web Audio API provided to us by MusicW. It re-exports the relevant
-- MusicW module, so typically just this module would be imported in other Estuary modules.


-- | Calculate how far ahead current system time is relative to audio context time.
-- (Subtract this from a system time to get an audio context time.)
-- (Add this to an audio context time to get a system time.)

audioClockDiff :: AudioContext -> IO NominalDiffTime
audioClockDiff ctx = diffUTCTime <$> getCurrentTime <*> getAudioTime ctx
