module Estuary.RenderState where

import Data.Time.Clock
import Data.Map
import qualified Sound.Tidal.Context as Tidal

import Estuary.Types.Definition

data RenderState = RenderState {
  logicalTime :: UTCTime,
  cachedDefs :: DefinitionMap,
  paramPatterns :: Map Int Tidal.ParamPattern,
  errors :: Map Int String,
  dirtEvents :: [(UTCTime,Tidal.ParamMap)],
  renderStartTime :: UTCTime,
  parseEndTime :: UTCTime,
  patternsToEventsEndTime :: UTCTime,
  renderEndTime :: UTCTime,
  renderTimes :: [NominalDiffTime],
  avgRenderTime :: NominalDiffTime
  }

initialRenderState :: UTCTime -> RenderState
initialRenderState t = RenderState {
  logicalTime = t,
  cachedDefs = empty,
  paramPatterns = empty,
  errors = empty,
  dirtEvents = [],
  renderStartTime = t,
  parseEndTime = t,
  patternsToEventsEndTime = t,
  renderEndTime = t,
  renderTimes = [],
  avgRenderTime = 0
  }
