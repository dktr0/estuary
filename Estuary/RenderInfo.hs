module Estuary.RenderInfo where

import Data.IntMap.Strict

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
