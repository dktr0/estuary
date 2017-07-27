module Estuary.Types.View where

import Text.JSON
import Estuary.Utility (firstKey)

data View =
  Views [View] |
  ViewDiv String View |
  LabelView Int |
  StructureView Int |
  TidalTextView Int |
  EvaluableTextView Int
  deriving (Show)

instance JSON View where
  showJSON (Views xs) = encJSDict [("Views",xs)]
  showJSON (ViewDiv c v) = encJSDict [("ViewDiv",showJSON c),("v",showJSON v)]
  showJSON (LabelView n) = encJSDict [("LabelView",n)]
  showJSON (StructureView n) = encJSDict [("StructureView",n)]
  showJSON (TidalTextView n) = encJSDict [("TidalTextView",n)]
  showJSON (EvaluableTextView n) = encJSDict [("EvaluableTextView",n)]
  readJSON (JSObject x) | firstKey x == "Views" = Views <$> valFromObj "Views" x
  readJSON (JSObject x) | firstKey x == "ViewDiv" = ViewDiv <$> valFromObj "ViewDiv" x <*> valFromObj "v" x
  readJSON (JSObject x) | firstKey x == "LabelView" = LabelView <$> valFromObj "LabelView" x
  readJSON (JSObject x) | firstKey x == "StructureView" = StructureView <$> valFromObj "StructureView" x
  readJSON (JSObject x) | firstKey x == "TidalTextView" = TidalTextView <$> valFromObj "TidalTextView" x
  readJSON (JSObject x) | firstKey x == "EvaluableTextView" = EvaluableTextView <$> valFromObj "EvaluableTextView" x
  readJSON (JSObject x) | otherwise = Error $ "Unable to parse JSObject as Estuary.Protocol.View: " ++ (show x)
  readJSON _ = Error $ "Unable to parse non-JSObject as Estuary.Protocol.View"

defaultView :: View
defaultView = Views [
  ViewDiv "eightTopL" (Views [LabelView 1, StructureView 2]),
  ViewDiv "eightTopR" (Views [LabelView 3, StructureView 4]),
  ViewDiv "eightMiddleL" (Views [LabelView 5, TidalTextView 6]),
  ViewDiv "eightMiddleR" (Views [LabelView 7, TidalTextView 8]),
  ViewDiv "eightBottomL" (Views [LabelView 9, EvaluableTextView 10]),
  ViewDiv "eightBottomR" (Views [LabelView 11, EvaluableTextView 12])
  ]

