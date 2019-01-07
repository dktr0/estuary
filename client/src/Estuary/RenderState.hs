module Estuary.RenderState where

import Data.Time.Clock
import Data.IntMap.Strict
import qualified Sound.Tidal.Context as Tidal
import qualified Sound.Punctual.PunctualW as Punctual
import qualified Sound.Punctual.Evaluation as Punctual
import Sound.MusicW.AudioContext

import Estuary.Types.Definition
import Estuary.RenderInfo
import Estuary.Types.CanvasOp
import qualified Estuary.Languages.SuperContinent as SuperContinent

data RenderState = RenderState {
  logicalTime :: !UTCTime,
  cachedDefs :: !DefinitionMap,
  paramPatterns :: !(IntMap Tidal.ControlPattern),
  dirtEvents :: ![(UTCTime,Tidal.ControlMap)],
  punctuals :: !(IntMap (Punctual.PunctualW AudioContextIO)),
  punctualVideo :: !(IntMap Punctual.PunctualState),
  superContinentProgram :: SuperContinent.Program,
  superContinentState :: SuperContinent.SuperContinentState,
  renderStartTime :: !UTCTime,
  renderEndTime :: !UTCTime,
  renderTimes :: ![NominalDiffTime],
  info :: !RenderInfo,
  canvasOps :: [(UTCTime,CanvasOp)]
  }

initialRenderState :: UTCTime -> IO RenderState
initialRenderState t = do
  scs <- SuperContinent.emptyState
  return $ RenderState {
    logicalTime = t,
    cachedDefs = empty,
    paramPatterns = empty,
    dirtEvents = [],
    punctuals = empty,
    punctualVideo = empty,
    superContinentProgram = [],
    superContinentState = scs,
    renderStartTime = t,
    renderEndTime = t,
    renderTimes = [],
    info = emptyRenderInfo,
    canvasOps = []
  }
