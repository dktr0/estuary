module Estuary.RenderInfo where

import Data.IntMap.Strict
import Data.Time.Clock
import Data.Text (Text)

import Estuary.Types.SvgOp

data RenderInfo = RenderInfo {
  errors :: !(IntMap Text),
  avgRenderLoad :: !Int,
  avgAnimationLoad :: !Int,
  avgZoneRenderTime :: !(IntMap Double),
  avgZoneAnimationTime :: !(IntMap Double),
  svgOps :: Maybe [SvgOp]
  } deriving (Show)

emptyRenderInfo :: RenderInfo
emptyRenderInfo = RenderInfo {
  errors = empty,
  avgRenderLoad = 0,
  avgAnimationLoad = 0,
  avgZoneRenderTime = empty,
  avgZoneAnimationTime = empty,
  svgOps = Nothing
  }
