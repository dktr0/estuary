module Estuary.RenderState where

import Data.Time.Clock
import Data.IntMap.Strict
import qualified Sound.Tidal.Context as Tidal

import Estuary.Types.Definition

data RenderInfo = RenderInfo {
  errors :: !(IntMap String),
  avgRenderLoad :: !Int,
  peakRenderLoad :: !Int,
  avgParseLoad :: !Int,
  peakParseLoad :: !Int,
  avgPatternsLoad :: !Int,
  peakPatternsLoad :: !Int
  } deriving (Show)

emptyRenderInfo :: RenderInfo
emptyRenderInfo = RenderInfo {
  errors = empty,
  avgRenderLoad = 0,
  peakRenderLoad = 0,
  avgParseLoad = 0,
  peakParseLoad = 0,
  avgPatternsLoad = 0,
  peakPatternsLoad = 0
  }

data RenderState = RenderState {
  logicalTime :: !UTCTime,
  cachedDefs :: !DefinitionMap,
  paramPatterns :: !(IntMap Tidal.ParamPattern),
  dirtEvents :: ![(UTCTime,Tidal.ParamMap)],
  renderStartTime :: !UTCTime,
  parseEndTime :: !UTCTime,
  patternsEndTime :: !UTCTime,
  renderEndTime :: !UTCTime,
  renderTimes :: ![NominalDiffTime],
  parseTimes :: ![NominalDiffTime],
  patternsTimes :: ![NominalDiffTime],
  info :: !RenderInfo
  }

initialRenderState :: UTCTime -> RenderState
initialRenderState t = RenderState {
  logicalTime = t,
  cachedDefs = empty,
  paramPatterns = empty,
  dirtEvents = [],
  renderStartTime = t,
  parseEndTime = t,
  patternsEndTime = t,
  renderEndTime = t,
  renderTimes = [],
  parseTimes = [],
  patternsTimes = [],
  info = emptyRenderInfo
  }
