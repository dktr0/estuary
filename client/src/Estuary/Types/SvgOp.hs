module Estuary.Types.SvgOp where

import Estuary.Types.Color
import Estuary.Types.Stroke
import Estuary.Types.Transform


data SvgOp =
  Line Double Double Double Double Stroke  |
  Rect Double Double Double Double Color Stroke  |
  Circle Double Double Double Color Stroke  |
  Ellipse Double Double Double Double Color Stroke  |
  Triangle Double Double Double Double Double Double Color Stroke  |
  Polyline [Double] Color Stroke  |
  Polygon [Double] Color Stroke 

  deriving (Show,Eq)
