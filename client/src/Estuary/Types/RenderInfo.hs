module Estuary.Types.RenderInfo where

import Data.IntMap.Strict
import Data.Time.Clock
import Data.Text (Text)

data RenderInfo = RenderInfo {
  errors :: !(IntMap Text),
  avgRenderLoad :: !Int,
  animationFPS :: !Int,
  animationLoad :: !Int, -- average time in milliseconds it takes to draw a frame
  avgZoneRenderTime :: !(IntMap Double),
  avgZoneAnimationTime :: !(IntMap Double),
  clockRatio :: !Double,
  clockRatioProblem :: !Bool,
  webDirtVoices :: !Int
  } deriving (Show)

emptyRenderInfo :: RenderInfo
emptyRenderInfo = RenderInfo {
  errors = empty,
  avgRenderLoad = 0,
  animationFPS = 0,
  animationLoad = 0,
  avgZoneRenderTime = empty,
  avgZoneAnimationTime = empty,
  clockRatio = 1,
  clockRatioProblem = False,
  webDirtVoices = 0
  }
