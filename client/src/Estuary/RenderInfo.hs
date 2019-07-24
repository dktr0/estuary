module Estuary.RenderInfo where

import Data.IntMap.Strict
import Data.Time.Clock

import Estuary.Types.SvgOp

data RenderInfo = RenderInfo {
  errors :: !(IntMap String),
  avgRenderLoad :: !Int,
  peakRenderLoad :: !Int,
  avgAnimationLoad :: !Int,
  avgZoneRenderTime :: !(IntMap Double),
  peakZoneRenderTime :: !(IntMap Double),
  avgZoneAnimationTime :: !(IntMap Double),
  peakZoneAnimationTime :: !(IntMap Double),
  svgOps :: Maybe [SvgOp]
  } deriving (Show)

emptyRenderInfo :: RenderInfo
emptyRenderInfo = RenderInfo {
  errors = empty,
  avgRenderLoad = 0,
  peakRenderLoad = 0,
  avgAnimationLoad = 0,
  avgZoneRenderTime = empty,
  peakZoneRenderTime = empty,
  avgZoneAnimationTime = empty,
  peakZoneAnimationTime = empty,
  svgOps = Nothing
  }
