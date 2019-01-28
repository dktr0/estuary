module Estuary.Types.CanvasOp where

import Estuary.Types.Color

data CanvasOp =
  Clear !Double |
  MoveTo !Double !Double |
  LineTo !Double !Double |
  Rect !Double !Double !Double !Double |
  Tri !Double !Double !Double !Double !Double !Double |
  StrokeStyle !Color |
  FillStyle !Color
  deriving (Show,Eq)

toActualWandH :: Int -> Int -> CanvasOp -> CanvasOp
toActualWandH w h (Clear a) = Clear a
toActualWandH w h (MoveTo x y) = MoveTo (mr w x) (mr h y)
toActualWandH w h (LineTo x y) = LineTo (mr w x) (mr h y)
toActualWandH w h (Rect x y w' h') = Rect (mr w x) (mr h y) (mr w w') (mr h h')
toActualWandH w h (Tri x0 y0 x1 y1 x2 y2) = Tri (mr w x0) (mr h y0) (mr w x1) (mr h y1) (mr w x2) (mr h y2)
toActualWandH w h (StrokeStyle s) = StrokeStyle s
toActualWandH w h (FillStyle s) = FillStyle s

mr :: Int -> Double -> Double
mr i d = fromIntegral $ round $ fromIntegral i * d / 100
