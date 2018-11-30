module Estuary.RenderInfo where

import Data.IntMap.Strict
import Data.Time.Clock

import Estuary.Types.SvgOp

data RenderInfo = RenderInfo {
  errors :: !(IntMap String),
  avgRenderLoad :: !Int,
  peakRenderLoad :: !Int,
  svgOps :: Maybe [SvgOp]
  }

emptyRenderInfo :: RenderInfo
emptyRenderInfo = RenderInfo {
  errors = empty,
  avgRenderLoad = 0,
  peakRenderLoad = 0,
  svgOps = Nothing
  }
