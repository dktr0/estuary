module Estuary.Types.RenderInfo where

import Data.IntMap.Strict
import Data.Time.Clock
import Data.Text (Text)

data RenderInfo = RenderInfo {
  errors :: !(IntMap Text),
  avgRenderLoad :: !Int,
  avgAnimationLoad :: !Int,
  avgZoneRenderTime :: !(IntMap Double),
  avgZoneAnimationTime :: !(IntMap Double),
  clockRatio :: !Double,
  clockRatioProblem :: !Bool
  } deriving (Show)

emptyRenderInfo :: RenderInfo
emptyRenderInfo = RenderInfo {
  errors = empty,
  avgRenderLoad = 0,
  avgAnimationLoad = 0,
  avgZoneRenderTime = empty,
  avgZoneAnimationTime = empty,
  clockRatio = 1,
  clockRatioProblem = False
  }
