module Estuary.Types.AnimationState where

import Data.Time

-- a type to represent rendering state info that is only written by the animationFrame
-- "thread"

data AnimationState = AnimationState {
  animationWakeTimeSystem :: !UTCTime
}
