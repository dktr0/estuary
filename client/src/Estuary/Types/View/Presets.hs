{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.View.Presets where

import Reflex.Dom
import Data.Map.Strict
import Data.Text
import qualified Data.Text as T


import Estuary.Types.View

-- presetViewsWDefinedColumnsAndRows :: View
-- presetViewsWDefinedColumnsAndRows =  divClass "test" emptyView

-- presetViewsWDefinedColumnsAndRows' :: View
-- presetViewsWDefinedColumnsAndRows' =  elAttr "div" ("class" =: "subGrid-container" <> "style" =: (setColumnsAndRows 2 3) ) emptyView


presetViews :: Map Text View

-- note: if an ensemble publishes a view called 'default', that view rather than this one,
-- will effectively be the default view in that ensemble.

presetViews = fromList [


      ("default",  GridView 2 3  [
      BorderDiv (Views [LabelView 1, SequenceView 2]),
      BorderDiv (Views [LabelView 3, SequenceView 4]),
      (Views [LabelView 5, TextView 6 5]),
      (Views [LabelView 7, TextView 8 5]),
      BorderDiv (Views [LabelView 9, TextView 10 5]),
      BorderDiv (Views [LabelView 11, TextView 12 5])
      ]),

      ("fulltexteditor", GridView 1 1 [
      ViewDiv "singleViewContainer" (Views [LabelView 0,TextView 1 20])
      ]),

      ("twocolumns", GridView 2 1  [
      ViewDiv "singleViewContainer" (TextView 1 30),
      ViewDiv "singleViewContainer" (TextView 2 30)
      ]),

      ("iclc2017", GridView 3 6  [
      ViewDiv "singleViewContainer" (Views [LabelView 0,TextView 1 2]),
      ViewDiv "singleViewContainer" (Views [LabelView 2,TextView 3 2]),
      ViewDiv "singleViewContainer" (Views [LabelView 4,TextView 5 2]),
      ViewDiv "singleViewContainer" (Views [LabelView 6,TextView 7 2]),
      ViewDiv "singleViewContainer" (Views [LabelView 8,TextView 9 2]),
      ViewDiv "singleViewContainer" (Views [LabelView 10,TextView 11 2]),
      ViewDiv "singleViewContainer" (Views [LabelView 12,TextView 13 2]),
      ViewDiv "singleViewContainer" (Views [LabelView 14,TextView 15 2]),
      ViewDiv "singleViewContainer" (Views [LabelView 16,TextView 17 2]),
      ViewDiv "singleViewContainer" (Views [LabelView 18,TextView 19 2]),
      ViewDiv "singleViewContainer" (Views [LabelView 20,TextView 21 2]),
      ViewDiv "singleViewContainer" (Views [LabelView 22,TextView 23 2]),
      ViewDiv "singleViewContainer" (Views [LabelView 24,TextView 25 2]),
      ViewDiv "singleViewContainer" (Views [LabelView 26,TextView 27 2]),
      ViewDiv "singleViewContainer" (Views [LabelView 28,TextView 29 2]),
      ViewDiv "singleViewContainer" (Views [LabelView 30,TextView 31 2]),
      ViewDiv "singleViewContainer" (Views [LabelView 32,TextView 33 2]),
      ViewDiv "singleViewContainer" (Views [LabelView 34,TextView 35 2])
      ]),

        ("Lima",  GridView 2 4 [
        ViewDiv "singleViewContainer" (Views [LabelView 0,TextView 1 5]),
        ViewDiv "singleViewContainer" (Views [LabelView 2,TextView 3 5]),
        ViewDiv "singleViewContainer" (Views [LabelView 4,TextView 5 5]),
        ViewDiv "singleViewContainer" (Views [LabelView 6,TextView 7 5]),
        ViewDiv "singleViewContainer" (Views [LabelView 8,TextView 9 5]),
        ViewDiv "singleViewContainer" (Views [LabelView 10,TextView 11 5]),
        ViewDiv "singleViewContainer" (Views [LabelView 12,TextView 13 5]),
        ViewDiv "singleViewContainer" (Views [LabelView 14,TextView 15 5])
      ]),

      ("RGGTRN", GridView 2 2 [
      ViewDiv "singleViewContainer" (Views [LabelView 0,TextView 1 10]),
      ViewDiv "singleViewContainer" (Views [LabelView 2,TextView 3 10]),
      ViewDiv "singleViewContainer" (Views [LabelView 4,TextView 5 10]),
      ViewDiv "singleViewContainer" (Views [LabelView 6,TextView 7 10])
      ]),

      ("cybernetic",GridView 2 2 [
      ViewDiv "singleViewContainer" (Views [LabelView 1, SequenceView 2]),
      ViewDiv "singleViewContainer" (Views [LabelView 3, TextView 4 4]),
      ViewDiv "singleViewContainer" (Views [LabelView 5, TextView 6 4]),
      ViewDiv "singleViewContainer" (Views [LabelView 7, TextView 8 4])
      ]),

      ("supercontinent",Views [
      ViewDiv "singleViewContainer" (Views [LabelView 0,TextView 1 5]),
      ViewDiv "singleViewContainer" (Views [LabelView 2,TextView 3 5]),
      ViewDiv "singleViewContainer" (Views [LabelView 4,TextView 5 5]),
      ViewDiv "singleViewContainer" (Views [LabelView 6,TextView 7 5]),
      ViewDiv "singleViewContainer" (Views [LabelView 8,TextView 9 5]),
      ViewDiv "singleViewContainer" (Views [LabelView 10,TextView 11 5]),
      ViewDiv "singleViewContainer" (Views [LabelView 12,TextView 13 5]),
      ViewDiv "singleViewContainer" (Views [LabelView 14,TextView 15 5]),
      ViewDiv "singleViewContainer" (Views [LabelView 16,TextView 17 10]),
      ViewDiv "singleViewContainer" EnsembleStatusView
      ]),

      ("iclc2019",GridView 2 2 [
      ViewDiv "singleViewContainer" (Views [LabelView 1, SequenceView 2]),
      ViewDiv "singleViewContainer" (Views [LabelView 3, TextView 4 10]),
      ViewDiv "singleViewContainer" (Views [LabelView 5, TextView 6 10]),
      ViewDiv "singleViewContainer" (Views [LabelView 7, TextView 8 10])
      ]),

      ("blackBox",GridView 1 1 [
         ViewDiv "singleViewContainer" (Views [LabelView 0,TextView 1 10]),
         ViewDiv "singleViewContainer" (Views [LabelView 2,TextView 3 10])
         ]),

      ("memorias",GridView 2 3 [
         ViewDiv "singleViewContainer" (Views [LabelView 0,TextView 1 5]),
         ViewDiv "singleViewContainer" (Views [LabelView 2,TextView 3 5]),
         ViewDiv "singleViewContainer" (Views [LabelView 4,TextView 5 5]),
         ViewDiv "singleViewContainer" (Views [LabelView 6,TextView 7 5]),
         ViewDiv "singleViewContainer" (Views [LabelView 8,TextView 9 5]),
         ViewDiv "singleViewContainer" (Views [LabelView 10,TextView 11 5])
         ]),

             
         ("supercontinent",GridView 2 3 [
            ViewDiv "eightMiddleL" (Views [LabelView 0,TextView 1 5]),
            ViewDiv "eightMiddleR" (Views [LabelView 2,TextView 3 5]),
            ViewDiv "eightMiddleL" (Views [LabelView 4,TextView 5 5]),
            ViewDiv "eightMiddleR" (Views [LabelView 6,TextView 7 5]),
            ViewDiv "eightMiddleL" (Views [LabelView 8,TextView 9 5]),
            ViewDiv "eightMiddleR" (Views [LabelView 10,TextView 11 5]),
            ViewDiv "eightMiddleL" (Views [LabelView 12,TextView 13 5]),
            ViewDiv "eightMiddleR" (Views [LabelView 14,TextView 15 5]),
            ViewDiv "eightMiddleL" (Views [LabelView 16,TextView 17 10]),
            ViewDiv "eightMiddleR" EnsembleStatusView
            ])
      ]
