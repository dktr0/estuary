module Estuary.Types.CanvasOp where

import Estuary.Types.Color

data CanvasOp =
  MoveTo Double Double |
  LineTo Double Double |
  Rect Double Double Double Double |
  StrokeStyle Color
  deriving (Show,Eq)
