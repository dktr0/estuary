module Estuary.Render.RenderOp where

-- A RenderOp represents a command that can be sent to Estuary's render engine
-- putRenderOp from Estuary.Widgets.R is used to send a command to the render engine
-- (and the render engine uses takeRenderOps to receive those commands)

import Estuary.Types.Definition
import Estuary.Types.Tempo

data RenderOp =
  WriteTempo Tempo |
  WriteZone Int Definition |
  ResetAllZones
