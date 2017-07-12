module Estuary.Types.View where

import Text.JSON
import Estuary.Utility (firstKey)

data View =
  Views [View] |
  LabelView Int |
  StructureView Int |
  TidalTextView Int |
  EvaluableTextView Int
  deriving (Show)

instance JSON View where
  showJSON (Views xs) = encJSDict [("Views",xs)]
  showJSON (LabelView n) = encJSDict [("LabelView",n)]
  showJSON (StructureView n) = encJSDict [("StructureView",n)]
  showJSON (TidalTextView n) = encJSDict [("TidalTextView",n)]
  showJSON (EvaluableTextView n) = encJSDict [("EvaluableTextView",n)]
  readJSON (JSObject x) | firstKey x == "Views" = Views <$> valFromObj "Views" x
  readJSON (JSObject x) | firstKey x == "LabelView" = LabelView <$> valFromObj "LabelView" x
  readJSON (JSObject x) | firstKey x == "StructureView" = StructureView <$> valFromObj "StructureView" x
  readJSON (JSObject x) | firstKey x == "TidalTextView" = TidalTextView <$> valFromObj "TidalTextView" x
  readJSON (JSObject x) | firstKey x == "EvaluableTextView" = EvaluableTextView <$> valFromObj "EvaluableTextView" x
  readJSON (JSObject x) | otherwise = Error $ "Unable to parse JSObject as Estuary.Protocol.View: " ++ (show x)
  readJSON _ = Error $ "Unable to parse non-JSObject as Estuary.Protocol.View"

defaultView :: View
defaultView = Views [
  LabelView 1, StructureView 2,
  LabelView 3, StructureView 4,
  LabelView 5, TidalTextView 6,
  LabelView 7, TidalTextView 8,
  LabelView 9, EvaluableTextView 10,
  LabelView 11, EvaluableTextView 12
  ]
