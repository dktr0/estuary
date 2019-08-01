module Estuary.Types.RenderState where

import Data.Time.Clock
import Data.IntMap.Strict
import qualified Sound.Tidal.Context as Tidal
import qualified Sound.Punctual.PunctualW as Punctual
import qualified Sound.Punctual.WebGL as Punctual
import Sound.MusicW.AudioContext
import GHCJS.DOM.Types

import Estuary.Types.Definition
import Estuary.Types.RenderInfo
import qualified Estuary.Languages.CineCer0.CineCer0State as CineCer0
import Estuary.Types.MovingAverage

data RenderState = RenderState {
  logicalTime :: !UTCTime,
  cachedDefs :: !DefinitionMap,
  cachedCanvasElement :: !(Maybe HTMLCanvasElement),
  paramPatterns :: !(IntMap Tidal.ControlPattern),
  dirtEvents :: ![(UTCTime,Tidal.ControlMap)],
  punctuals :: !(IntMap (Punctual.PunctualW AudioContextIO)),
  punctualWebGLs :: !(IntMap Punctual.PunctualWebGL),
  cineCer0States :: !(IntMap CineCer0.CineCer0State),
  renderStartTime :: !UTCTime,
  renderEndTime :: !UTCTime,
  renderTime :: !MovingAverage,
  zoneRenderTimes :: !(IntMap MovingAverage),
  zoneAnimationTimes :: !(IntMap MovingAverage),
  info :: !RenderInfo
  }

initialRenderState :: UTCTime -> IO RenderState
initialRenderState t = do
  return $ RenderState {
    logicalTime = t,
    cachedDefs = empty,
    cachedCanvasElement = Nothing,
    paramPatterns = empty,
    dirtEvents = [],
    punctuals = empty,
    punctualWebGLs = empty,
    cineCer0States = empty,
    renderStartTime = t,
    renderEndTime = t,
    renderTime = newAverage 20,
    zoneRenderTimes = empty,
    zoneAnimationTimes = empty,
    info = emptyRenderInfo
  }
