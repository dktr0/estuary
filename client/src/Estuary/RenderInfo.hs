module Estuary.RenderInfo where

import Data.IntMap.Strict
import Estuary.Types.SvgOp
import Estuary.Types.CanvasOp

data RenderInfo = RenderInfo {
  errors :: !(IntMap String),
  avgRenderLoad :: !Int,
  peakRenderLoad :: !Int,
  svgOps :: Maybe [SvgOp],
  canvasOps :: [CanvasOp]
  } deriving (Show)

emptyRenderInfo :: RenderInfo
emptyRenderInfo = RenderInfo {
  errors = empty,
  avgRenderLoad = 0,
  peakRenderLoad = 0,
  svgOps = Nothing,
  canvasOps = []
  }
