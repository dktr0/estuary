{-# LANGUAGE DeriveDataTypeable #-}

module Estuary.Types.View where

import Text.JSON
import Text.JSON.Generic
import Estuary.Utility (firstKey)

data View =
  Views [View] |
  ViewDiv String View |
  LabelView Int |
  StructureView Int |
  TextView Int Int | -- first int is zone to edit, second int is number of lines in editor
  SequenceView Int |
  SvgDisplayView Int | -- Int is z-index
  CanvasDisplayView Int -- Int is z-index
  deriving (Show,Eq,Data,Typeable)

instance JSON View where
  showJSON = toJSON
  readJSON = fromJSON

standardView :: View
standardView = Views [
  ViewDiv "eightTopL" (Views [LabelView 1, StructureView 2]),
  ViewDiv "eightTopR" (Views [LabelView 3, StructureView 4]),
  ViewDiv "eightMiddleL" (Views [LabelView 5, TextView 6 3]),
  ViewDiv "eightMiddleR" (Views [LabelView 7, TextView 8 3]),
  ViewDiv "eightBottomL" (Views [LabelView 9, TextView 10 3]),
  ViewDiv "eightBottomR" (Views [LabelView 11, SequenceView 12]),
  SvgDisplayView (-2),
  CanvasDisplayView (-1)
  ]

emptyView :: View
emptyView = Views []

presetView :: String -> View

presetView "fulltexteditor" = Views [
  ViewDiv "eightMiddleL" (Views [LabelView 0,TextView 1 2]),
  SvgDisplayView (-2),
  CanvasDisplayView (-1)
  ]


presetView "iclc2017" = Views [
  ViewDiv "eightMiddleL" (Views [LabelView 0,TextView 1 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 2,TextView 3 2]),
  ViewDiv "eightMiddleL" (Views [LabelView 4,TextView 5 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 6,TextView 7 2]),
  ViewDiv "eightMiddleL" (Views [LabelView 8,TextView 9 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 10,TextView 11 2]),
  ViewDiv "eightMiddleL" (Views [LabelView 12,TextView 13 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 14,TextView 15 2]),
  ViewDiv "eightMiddleL" (Views [LabelView 16,TextView 17 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 18,TextView 19 2]),
  ViewDiv "eightMiddleL" (Views [LabelView 20,TextView 21 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 22,TextView 23 2]),
  ViewDiv "eightMiddleL" (Views [LabelView 24,TextView 25 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 26,TextView 27 2]),
  ViewDiv "eightMiddleL" (Views [LabelView 28,TextView 29 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 30,TextView 31 2]),
  ViewDiv "eightMiddleL" (Views [LabelView 32,TextView 33 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 34,TextView 35 2]),
  SvgDisplayView (-2),
  CanvasDisplayView (-1)
  ]

presetView "Bogota" = Views [
  ViewDiv "eightMiddleL" (Views [LabelView 0,TextView 1 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 2,TextView 3 2]),
  ViewDiv "eightMiddleL" (Views [LabelView 4,TextView 5 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 6,TextView 7 2]),
  ViewDiv "eightMiddleL" (Views [LabelView 8,TextView 9 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 10,TextView 11 2]),
  ViewDiv "eightMiddleL" (Views [LabelView 12,TextView 13 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 14,TextView 15 2]),
  ViewDiv "eightMiddleL" (Views [LabelView 16,TextView 17 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 18,TextView 19 2]),
  ViewDiv "eightMiddleL" (Views [LabelView 20,TextView 21 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 22,TextView 23 2]),
  ViewDiv "eightMiddleL" (Views [LabelView 24,TextView 25 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 26,TextView 27 2]),
  ViewDiv "eightMiddleL" (Views [LabelView 28,TextView 29 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 30,TextView 31 2]),
  ViewDiv "eightMiddleL" (Views [LabelView 32,TextView 33 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 34,TextView 35 2]),
  SvgDisplayView (-2),
  CanvasDisplayView (-1)
  ]

presetView "Manizales" = Views [
  ViewDiv "eightMiddleL" (Views [LabelView 0,TextView 1 10]),
  ViewDiv "eightMiddleR" (Views [LabelView 2,TextView 3 10]),
  ViewDiv "eightMiddleL" (Views [LabelView 4,TextView 5 10]),
  ViewDiv "eightMiddleR" (Views [LabelView 6,TextView 7 10]),
  SvgDisplayView (-2),
  CanvasDisplayView (-1)
  ]

presetView "Medellin" = presetView "Manizales"

presetView "Lima" = Views [
    ViewDiv "eightMiddleL" (Views [LabelView 0,TextView 1 5]),
    ViewDiv "eightMiddleR" (Views [LabelView 2,TextView 3 5]),
    ViewDiv "eightMiddleL" (Views [LabelView 4,TextView 5 5]),
    ViewDiv "eightMiddleR" (Views [LabelView 6,TextView 7 5]),
    ViewDiv "eightMiddleL" (Views [LabelView 8,TextView 9 5]),
    ViewDiv "eightMiddleR" (Views [LabelView 10,TextView 11 5]),
    ViewDiv "eightMiddleL" (Views [LabelView 12,TextView 13 5]),
    ViewDiv "eightMiddleR" (Views [LabelView 14,TextView 15 5]),
    SvgDisplayView (-2),
    CanvasDisplayView (-1)
    ]

presetView "Uio" = Views [
    ViewDiv "eightMiddleL" (Views [LabelView 0,TextView 1 2]),
    ViewDiv "eightMiddleR" (Views [LabelView 2,TextView 3 2]),
    ViewDiv "eightMiddleL" (Views [LabelView 4,TextView 5 2]),
    ViewDiv "eightMiddleR" (Views [LabelView 6,TextView 7 2]),
    ViewDiv "eightMiddleL" (Views [LabelView 8,TextView 9 2]),
    ViewDiv "eightMiddleR" (Views [LabelView 10,TextView 11 2]),
    ViewDiv "eightMiddleL" (Views [LabelView 12,TextView 13 2]),
    ViewDiv "eightMiddleR" (Views [LabelView 14,TextView 15 2]),
    ViewDiv "eightMiddleL" (Views [LabelView 16,TextView 17 2]),
    ViewDiv "eightMiddleR" (Views [LabelView 18,TextView 19 2]),
    ViewDiv "eightMiddleL" (Views [LabelView 20,TextView 21 2]),
    ViewDiv "eightMiddleR" (Views [LabelView 22,TextView 23 2]),
    ViewDiv "eightMiddleL" (Views [LabelView 24,TextView 25 2]),
    ViewDiv "eightMiddleR" (Views [LabelView 26,TextView 27 2]),
    ViewDiv "eightMiddleL" (Views [LabelView 28,TextView 29 2]),
    ViewDiv "eightMiddleR" (Views [LabelView 30,TextView 31 2]),
    ViewDiv "eightMiddleL" (Views [LabelView 32,TextView 33 2]),
    ViewDiv "eightMiddleR" (Views [LabelView 34,TextView 35 2]),
    SvgDisplayView (-2),
    CanvasDisplayView (-1)
    ]

presetView "RGGTRN" = Views [
  ViewDiv "eightMiddleL" (Views [LabelView 0,TextView 1 10]),
  ViewDiv "eightMiddleR" (Views [LabelView 2,TextView 3 10]),
  ViewDiv "eightMiddleL" (Views [LabelView 4,TextView 5 10]),
  ViewDiv "eightMiddleR" (Views [LabelView 6,TextView 7 10]),
  SvgDisplayView (-2),
  CanvasDisplayView (-1)
  ]

presetView "cybernetic" = Views [
  ViewDiv "fullRowTopOrBottom" (Views [LabelView 1, SequenceView 2]),
  ViewDiv "fullRowMiddle" (Views [LabelView 3, TextView 4 4]),
  ViewDiv "fullRowMiddle" (Views [LabelView 5, TextView 6 4]),
  ViewDiv "fullRowMiddle" (Views [LabelView 7, TextView 8 4]),
  SvgDisplayView (-2),
  CanvasDisplayView (-1)
  ]

presetView "iclc2019" = Views [
  ViewDiv "eightMiddleL" (Views [LabelView 1, SequenceView 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 3, TextView 4 10]),
  ViewDiv "eightMiddleL" (Views [LabelView 5, TextView 6 10]),
  ViewDiv "eightMiddleR" (Views [LabelView 7, TextView 8 10]),
  SvgDisplayView (-2),
  CanvasDisplayView (-1)
  ]

presetView "iclc2019noVisuals" = Views [
  ViewDiv "eightMiddleL" (Views [LabelView 1, SequenceView 2]),
  ViewDiv "eightMiddleR" (Views [LabelView 3, TextView 4 10]),
  ViewDiv "eightMiddleL" (Views [LabelView 5, TextView 6 10]),
  ViewDiv "eightMiddleR" (Views [LabelView 7, TextView 8 10])
  ]

presetView _ = standardView
