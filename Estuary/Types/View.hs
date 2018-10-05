module Estuary.Types.View where

import Text.ParserCombinators.Parsec
import Text.JSON
import Estuary.Utility (firstKey)

data View =
  Views [View] |
  ViewDiv String View |
  LabelView Int |
  StructureView Int |
  TidalTextView Int Int | -- first int is zone to edit, second int is number of lines in editor
  EvaluableTextView Int |
  SvgDisplayView
  deriving (Show,Eq)

instance JSON View where
  showJSON (Views xs) = encJSDict [("Views",xs)]
  showJSON (ViewDiv c v) = encJSDict [("ViewDiv",showJSON c),("v",showJSON v)]
  showJSON (LabelView n) = encJSDict [("LabelView",n)]
  showJSON (StructureView n) = encJSDict [("StructureView",n)]
  showJSON (TidalTextView n rows) = encJSDict [("TidalTextView",n),("rows",rows)]
  showJSON (EvaluableTextView n) = encJSDict [("EvaluableTextView",n)]
  showJSON (SvgDisplayView) = encJSDict [("SvgDisplayView",0::Int)]
  readJSON (JSObject x) | firstKey x == "Views" = Views <$> valFromObj "Views" x
  readJSON (JSObject x) | firstKey x == "ViewDiv" = ViewDiv <$> valFromObj "ViewDiv" x <*> valFromObj "v" x
  readJSON (JSObject x) | firstKey x == "LabelView" = LabelView <$> valFromObj "LabelView" x
  readJSON (JSObject x) | firstKey x == "StructureView" = StructureView <$> valFromObj "StructureView" x
  readJSON (JSObject x) | firstKey x == "TidalTextView" = TidalTextView <$> valFromObj "TidalTextView" x <*> valFromObj "rows" x
  readJSON (JSObject x) | firstKey x == "EvaluableTextView" = EvaluableTextView <$> valFromObj "EvaluableTextView" x
  readJSON (JSObject x) | firstKey x == "SvgDisplayView" = return $ SvgDisplayView
  readJSON (JSObject x) | otherwise = Error $ "Unable to parse JSObject as Estuary.Protocol.View: " ++ (show x)
  readJSON _ = Error $ "Unable to parse non-JSObject as Estuary.Protocol.View"

standardView :: View
standardView = Views [
  ViewDiv "eightTopL" (Views [LabelView 1, StructureView 2]),
  ViewDiv "eightTopR" (Views [LabelView 3, StructureView 4]),
  ViewDiv "eightMiddleL" (Views [LabelView 5, TidalTextView 6 3]),
  ViewDiv "eightMiddleR" (Views [LabelView 7, TidalTextView 8 3]),
  ViewDiv "eightBottomL" (Views [LabelView 9, TidalTextView 10 3]),
  ViewDiv "eightBottomR" (Views [LabelView 11, EvaluableTextView 12])
  ]

emptyView :: View
emptyView = Views []

viewsParser :: GenParser Char a View
viewsParser = do
  spaces
  many1 viewParser >>= return . Views

viewParser :: GenParser Char a View
viewParser = do
  v <- choice [
    try viewDiv,
    try labelView,
    try structureView,
    try tidalTextView,
    try evaluableTextView,
    svgDisplayView]
  spaces
  return v

viewDiv = between (char '{') (char '}') $ do
  spaces
  cssClass <- many1 alphaNum
  skipMany1 space
  vs <- viewsParser
  spaces
  return $ ViewDiv cssClass vs

labelView = string "label:" >> (read <$> many1 digit) >>= return . LabelView
structureView = string "structure:" >> (read <$> many1 digit) >>= return . StructureView
evaluableTextView = string "evaluable:" >> (read <$> many1 digit) >>= return . EvaluableTextView
tidalTextView = do
  string "tidal:"
  x <- read <$> many1 digit
  skipMany1 space
  y <- read <$> many1 digit
  return $ TidalTextView x y
svgDisplayView = string "svgDisplayView:" >> return SvgDisplayView

presetView :: String -> View

presetView "iclc2017" = Views [
  ViewDiv "eightMiddleL" (Views [LabelView 0,TidalTextView 1 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 2,TidalTextView 3 2]),
  ViewDiv "eightMiddleL" (Views [LabelView 4,TidalTextView 5 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 6,TidalTextView 7 2]),
  ViewDiv "eightMiddleL" (Views [LabelView 8,TidalTextView 9 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 10,TidalTextView 11 2]),
  ViewDiv "eightMiddleL" (Views [LabelView 12,TidalTextView 13 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 14,TidalTextView 15 2]),
  ViewDiv "eightMiddleL" (Views [LabelView 16,TidalTextView 17 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 18,TidalTextView 19 2]),
  ViewDiv "eightMiddleL" (Views [LabelView 20,TidalTextView 21 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 22,TidalTextView 23 2]),
  ViewDiv "eightMiddleL" (Views [LabelView 24,TidalTextView 25 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 26,TidalTextView 27 2]),
  ViewDiv "eightMiddleL" (Views [LabelView 28,TidalTextView 29 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 30,TidalTextView 31 2]),
  ViewDiv "eightMiddleL" (Views [LabelView 32,TidalTextView 33 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 34,TidalTextView 35 2])
  ]

presetView "Bogota" = Views [
  ViewDiv "eightMiddleL" (Views [LabelView 0,TidalTextView 1 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 2,TidalTextView 3 2]),
  ViewDiv "eightMiddleL" (Views [LabelView 4,TidalTextView 5 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 6,TidalTextView 7 2]),
  ViewDiv "eightMiddleL" (Views [LabelView 8,TidalTextView 9 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 10,TidalTextView 11 2]),
  ViewDiv "eightMiddleL" (Views [LabelView 12,TidalTextView 13 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 14,TidalTextView 15 2]),
  ViewDiv "eightMiddleL" (Views [LabelView 16,TidalTextView 17 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 18,TidalTextView 19 2]),
  ViewDiv "eightMiddleL" (Views [LabelView 20,TidalTextView 21 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 22,TidalTextView 23 2]),
  ViewDiv "eightMiddleL" (Views [LabelView 24,TidalTextView 25 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 26,TidalTextView 27 2]),
  ViewDiv "eightMiddleL" (Views [LabelView 28,TidalTextView 29 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 30,TidalTextView 31 2]),
  ViewDiv "eightMiddleL" (Views [LabelView 32,TidalTextView 33 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 34,TidalTextView 35 2])
  ]

presetView "Manizales" = Views [
  ViewDiv "eightMiddleL" (Views [LabelView 0,TidalTextView 1 10]),
  ViewDiv "eightMiddleR" (Views [LabelView 2,TidalTextView 3 10]),
  ViewDiv "eightMiddleL" (Views [LabelView 4,TidalTextView 5 10]),
  ViewDiv "eightMiddleR" (Views [LabelView 6,TidalTextView 7 10])
  ]

presetView "Medellin" = presetView "Manizales"

presetView "Lima" = Views [
    ViewDiv "eightMiddleL" (Views [LabelView 0,TidalTextView 1 5]),
    ViewDiv "eightMiddleR" (Views [LabelView 2,TidalTextView 3 5]),
    ViewDiv "eightMiddleL" (Views [LabelView 4,TidalTextView 5 5]),
    ViewDiv "eightMiddleR" (Views [LabelView 6,TidalTextView 7 5]),
    ViewDiv "eightMiddleL" (Views [LabelView 8,TidalTextView 9 5]),
    ViewDiv "eightMiddleR" (Views [LabelView 10,TidalTextView 11 5]),
    ViewDiv "eightMiddleL" (Views [LabelView 12,TidalTextView 13 5]),
    ViewDiv "eightMiddleR" (Views [LabelView 14,TidalTextView 15 5])
    ]

presetView "Uio" = Views [
    ViewDiv "eightMiddleL" (Views [LabelView 0,TidalTextView 1 2]),
    ViewDiv "eightMiddleR" (Views [LabelView 2,TidalTextView 3 2]),
    ViewDiv "eightMiddleL" (Views [LabelView 4,TidalTextView 5 2]),
    ViewDiv "eightMiddleR" (Views [LabelView 6,TidalTextView 7 2]),
    ViewDiv "eightMiddleL" (Views [LabelView 8,TidalTextView 9 2]),
    ViewDiv "eightMiddleR" (Views [LabelView 10,TidalTextView 11 2]),
    ViewDiv "eightMiddleL" (Views [LabelView 12,TidalTextView 13 2]),
    ViewDiv "eightMiddleR" (Views [LabelView 14,TidalTextView 15 2]),
    ViewDiv "eightMiddleL" (Views [LabelView 16,TidalTextView 17 2]),
    ViewDiv "eightMiddleR" (Views [LabelView 18,TidalTextView 19 2]),
    ViewDiv "eightMiddleL" (Views [LabelView 20,TidalTextView 21 2]),
    ViewDiv "eightMiddleR" (Views [LabelView 22,TidalTextView 23 2]),
    ViewDiv "eightMiddleL" (Views [LabelView 24,TidalTextView 25 2]),
    ViewDiv "eightMiddleR" (Views [LabelView 26,TidalTextView 27 2]),
    ViewDiv "eightMiddleL" (Views [LabelView 28,TidalTextView 29 2]),
    ViewDiv "eightMiddleR" (Views [LabelView 30,TidalTextView 31 2]),
    ViewDiv "eightMiddleL" (Views [LabelView 32,TidalTextView 33 2]),
    ViewDiv "eightMiddleR" (Views [LabelView 34,TidalTextView 35 2])
    ]


presetView "RGGTRN" = Views [
  ViewDiv "eightMiddleL" (Views [LabelView 0,TidalTextView 1 10]),
  ViewDiv "eightMiddleR" (Views [LabelView 2,TidalTextView 3 10]),
  ViewDiv "eightMiddleL" (Views [LabelView 4,TidalTextView 5 10]),
  ViewDiv "eightMiddleR" (Views [LabelView 6,TidalTextView 7 10])
  ]

presetView "working" = Views [
  ViewDiv "eightTopL" (Views [LabelView 1, StructureView 2]),
  ViewDiv "eightTopR" (Views [LabelView 3, SvgDisplayView]),
  ViewDiv "eightMiddleL" (Views [LabelView 5, TidalTextView 6 7]),
  ViewDiv "eightMiddleR" (Views [LabelView 7, TidalTextView 8 7]),
  ViewDiv "eightBottomL" (Views [LabelView 9, TidalTextView 10 7]),
  ViewDiv "eightBottomR" (Views [LabelView 11, EvaluableTextView 12])
  ]

presetView "cybernetic" = Views [
  ViewDiv "eightMiddleL" (Views [LabelView 1, TidalTextView 2 5]),
  ViewDiv "eightMiddleR" (Views [LabelView 3, TidalTextView 4 5]),
  ViewDiv "eightMiddleL" (Views [LabelView 5, TidalTextView 6 5]),
  ViewDiv "eightMiddleR" (Views [LabelView 7, TidalTextView 8 5]),
  ViewDiv "eightMiddleL" (Views [LabelView 9, TidalTextView 10 5]),
  ViewDiv "eightMiddleR" (Views [LabelView 11, TidalTextView 12 5]),
  ViewDiv "eightMiddleL" (Views [LabelView 13, TidalTextView 14 5]),
  ViewDiv "eightMiddleR" (Views [LabelView 15, TidalTextView 16 5]),
  ViewDiv "eightMiddleL" (Views [LabelView 17, TidalTextView 18 5]),
  ViewDiv "eightMiddleR" (Views [LabelView 19, TidalTextView 20 5])
  ]

presetView _ = standardView
