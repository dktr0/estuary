module Estuary.Render.ZoneOp where

-- A ZoneOp represents a command that can be sent to Estuary's render thread

import Estuary.Types.Definition
import Estuary.Types.Tempo

data ZoneOp =
  SetZoneOp Int Definition |
  ClearAllZonesOp
