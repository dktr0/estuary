module Estuary.Types.SvgOp where

import Estuary.Types.Color
import Estuary.Types.Stroke
import Estuary.Types.Transform


data SvgOp =
  Line Double Double Double Double Stroke  Transform|
  Rect Double Double Double Double  Color Stroke  Transform|
  Circle Double Double Double Color Stroke  Transform|
  Ellipse Double Double Double Double Color Stroke  Transform|
  Triangle Double Double Double Double Double Double Color Stroke Transform |
  Polyline [Double] Color Stroke Transform |
  Polygon [Double] Color Stroke Transform

  deriving (Show,Eq)
