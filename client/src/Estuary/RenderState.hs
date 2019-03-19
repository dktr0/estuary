module Estuary.RenderState where

import Data.Time.Clock
import Data.IntMap.Strict
import qualified Sound.Tidal.Context as Tidal
import qualified Sound.Punctual.PunctualW as Punctual
import qualified Sound.Punctual.WebGL as Punctual
import qualified Sound.Punctual.Evaluation as Punctual
import Sound.MusicW.AudioContext
import GHCJS.DOM.Types

import Estuary.Types.Definition
import Estuary.RenderInfo
import Estuary.Types.CanvasOp
import qualified Estuary.Languages.SuperContinent as SuperContinent
import Estuary.Types.MovingAverage

data RenderState = RenderState {
  logicalTime :: !UTCTime,
  cachedDefs :: !DefinitionMap,
  cachedCanvasElement :: !(Maybe HTMLCanvasElement),
  paramPatterns :: !(IntMap Tidal.ControlPattern),
  dirtEvents :: ![(UTCTime,Tidal.ControlMap)],
  punctuals :: !(IntMap (Punctual.PunctualW AudioContextIO)),
  punctualWebGLs :: !(IntMap Punctual.PunctualWebGL),
  punctualVideo :: !(IntMap Punctual.PunctualState),
  superContinentProgram :: SuperContinent.Program,
  superContinentState :: SuperContinent.SuperContinentState,
  renderStartTime :: !UTCTime,
  renderEndTime :: !UTCTime,
  renderTime :: !MovingAverage,
  info :: !RenderInfo,
  canvasOps :: [(UTCTime,CanvasOp)]
  }

initialRenderState :: UTCTime -> IO RenderState
initialRenderState t = do
  scs <- SuperContinent.emptyState
  return $ RenderState {
    logicalTime = t,
    cachedDefs = empty,
    cachedCanvasElement = Nothing,
    paramPatterns = empty,
    dirtEvents = [],
    punctuals = empty,
    punctualWebGLs = empty,
    punctualVideo = empty,
    superContinentProgram = [],
    superContinentState = scs,
    renderStartTime = t,
    renderEndTime = t,
    renderTime = newAverage 20,
    info = emptyRenderInfo,
    canvasOps = []
  }
