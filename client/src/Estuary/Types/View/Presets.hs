{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.View.Presets where

import Reflex.Dom
import Data.Map.Strict
import Data.Text
import qualified Data.Text as T


import Estuary.Types.View

presetViews :: Map Text View

-- note: if an ensemble publishes a view called 'default', that view rather than this one,
-- will effectively be the default view in that ensemble.

presetViews = fromList [

       ("def",  GridView 2 3  [
       (Views [LabelView 1, NotePadView 2]),
       (Views [LabelView 3, CodeView 4 0]),
       (Views [LabelView 5, CodeView 6 0]),
       (Views [LabelView 7, CodeView 8 0]),
       (Views [LabelView 9, CodeView 10 0]),
       (Views [LabelView 11, CodeView 12 0])
      ]),

      ("fulltexteditor", GridView 1 1 [
      BorderDiv [LabelView 0,CodeView 1 0]
      ]),

      ("twocolumns", GridView 2 1  [
      BorderDiv [LabelView 0,CodeView 1 0],
      BorderDiv [LabelView 2,CodeView 3 0]
      ]),

      ("twobytwo", GridView 2 2 [
      BorderDiv [LabelView 0,CodeView 1 0],
      BorderDiv [LabelView 2,CodeView 3 0],
      BorderDiv [LabelView 4,CodeView 5 0],
      BorderDiv [LabelView 6,CodeView 7 0]
      ]),

      ("twobythree", GridView 2 3 [
      BorderDiv [LabelView 0,CodeView 1 0],
      BorderDiv [LabelView 2,CodeView 3 0],
      BorderDiv [LabelView 4,CodeView 5 0],
      BorderDiv [LabelView 6,CodeView 7 0],
      BorderDiv [LabelView 8,CodeView 9 0],
      BorderDiv [LabelView 10,CodeView 11 0]
      ]),

      ("twobyfour",  GridView 2 4 [
        BorderDiv [LabelView 0,CodeView 1 0],
        BorderDiv [LabelView 2,CodeView 3 0],
        BorderDiv [LabelView 4,CodeView 5 0],
        BorderDiv [LabelView 6,CodeView 7 0],
        BorderDiv [LabelView 8,CodeView 9 0],
        BorderDiv [LabelView 10,CodeView 11 0],
        BorderDiv [LabelView 12,CodeView 13 0],
        BorderDiv [LabelView 14,CodeView 15 0]
      ]),

      ("twobyfive",  GridView 2 5 [
      BorderDiv [LabelView 0,CodeView 1 0],
      BorderDiv [LabelView 2,CodeView 3 0],
      BorderDiv [LabelView 4,CodeView 5 0],
      BorderDiv [LabelView 6,CodeView 7 0],
      BorderDiv [LabelView 8,CodeView 9 0],
      BorderDiv [LabelView 10,CodeView 11 0],
      BorderDiv [LabelView 12,CodeView 13 0],
      BorderDiv [LabelView 14,CodeView 15 0],
      BorderDiv [LabelView 15,CodeView 16 0],
      BorderDiv [LabelView 17,CodeView 18 0]
      ]),

      ("twobysix",  GridView 2 6 [
      BorderDiv [LabelView 0,CodeView 1 0],
      BorderDiv [LabelView 2,CodeView 3 0],
      BorderDiv [LabelView 4,CodeView 5 0],
      BorderDiv [LabelView 6,CodeView 7 0],
      BorderDiv [LabelView 8,CodeView 9 0],
      BorderDiv [LabelView 10,CodeView 11 0],
      BorderDiv [LabelView 12,CodeView 13 0],
      BorderDiv [LabelView 14,CodeView 15 0],
      BorderDiv [LabelView 15,CodeView 16 0],
      BorderDiv [LabelView 17,CodeView 18 0],
      BorderDiv [LabelView 19,CodeView 20 0],
      BorderDiv [LabelView 21,CodeView 22 0]
      ]),

      ("threebyfive",  GridView 3 5 [
      BorderDiv [LabelView 0,CodeView 1 0],
      BorderDiv [LabelView 2,CodeView 3 0],
      BorderDiv [LabelView 4,CodeView 5 0],
      BorderDiv [LabelView 6,CodeView 7 0],
      BorderDiv [LabelView 8,CodeView 9 0],
      BorderDiv [LabelView 10,CodeView 11 0],
      BorderDiv [LabelView 12,CodeView 13 0],
      BorderDiv [LabelView 14,CodeView 15 0],
      BorderDiv [LabelView 15,CodeView 16 0],
      BorderDiv [LabelView 17,CodeView 18 0],
      BorderDiv [LabelView 19,CodeView 20 0],
      BorderDiv [LabelView 21,CodeView 22 0],
      BorderDiv [LabelView 22,CodeView 23 0],
      BorderDiv [LabelView 24,CodeView 25 0],
      BorderDiv [LabelView 25,CodeView 26 0]
      ]),

      ("threebysix", GridView 3 6  [
      BorderDiv [LabelView 0,CodeView 1 0],
      BorderDiv [LabelView 2,CodeView 3 0],
      BorderDiv [LabelView 4,CodeView 5 0],
      BorderDiv [LabelView 6,CodeView 7 0],
      BorderDiv [LabelView 8,CodeView 9 0],
      BorderDiv [LabelView 10,CodeView 11 0],
      BorderDiv [LabelView 12,CodeView 13 0],
      BorderDiv [LabelView 14,CodeView 15 0],
      BorderDiv [LabelView 16,CodeView 17 0],
      BorderDiv [LabelView 18,CodeView 19 0],
      BorderDiv [LabelView 20,CodeView 21 0],
      BorderDiv [LabelView 22,CodeView 23 0],
      BorderDiv [LabelView 24,CodeView 25 0],
      BorderDiv [LabelView 26,CodeView 27 0],
      BorderDiv [LabelView 28,CodeView 29 0],
      BorderDiv [LabelView 30,CodeView 31 0],
      BorderDiv [LabelView 32,CodeView 33 0],
      BorderDiv [LabelView 34,CodeView 35 0]
      ]),

      ("threebyseven", GridView 3 7  [
      BorderDiv [LabelView 0,CodeView 1 0],
      BorderDiv [LabelView 2,CodeView 3 0],
      BorderDiv [LabelView 4,CodeView 5 0],
      BorderDiv [LabelView 6,CodeView 7 0],
      BorderDiv [LabelView 8,CodeView 9 0],
      BorderDiv [LabelView 10,CodeView 11 0],
      BorderDiv [LabelView 12,CodeView 13 0],
      BorderDiv [LabelView 14,CodeView 15 0],
      BorderDiv [LabelView 16,CodeView 17 0],
      BorderDiv [LabelView 18,CodeView 19 0],
      BorderDiv [LabelView 20,CodeView 21 0],
      BorderDiv [LabelView 22,CodeView 23 0],
      BorderDiv [LabelView 24,CodeView 25 0],
      BorderDiv [LabelView 26,CodeView 27 0],
      BorderDiv [LabelView 28,CodeView 29 0],
      BorderDiv [LabelView 30,CodeView 31 0],
      BorderDiv [LabelView 32,CodeView 33 0],
      BorderDiv [LabelView 34,CodeView 35 0],
      BorderDiv [LabelView 35,CodeView 36 0],
      BorderDiv [LabelView 37,CodeView 38 0]
      ]),

      ("fourbyseven", GridView 4 7  [
      BorderDiv [LabelView 0,CodeView 1 0],
      BorderDiv [LabelView 2,CodeView 3 0],
      BorderDiv [LabelView 4,CodeView 5 0],
      BorderDiv [LabelView 6,CodeView 7 0],
      BorderDiv [LabelView 8,CodeView 9 0],
      BorderDiv [LabelView 10,CodeView 11 0],
      BorderDiv [LabelView 12,CodeView 13 0],
      BorderDiv [LabelView 14,CodeView 15 0],
      BorderDiv [LabelView 16,CodeView 17 0],
      BorderDiv [LabelView 18,CodeView 19 0],
      BorderDiv [LabelView 20,CodeView 21 0],
      BorderDiv [LabelView 22,CodeView 23 0],
      BorderDiv [LabelView 24,CodeView 25 0],
      BorderDiv [LabelView 26,CodeView 27 0],
      BorderDiv [LabelView 28,CodeView 29 0],
      BorderDiv [LabelView 30,CodeView 31 0],
      BorderDiv [LabelView 32,CodeView 33 0],
      BorderDiv [LabelView 34,CodeView 35 0],
      BorderDiv [LabelView 36,CodeView 37 0],
      BorderDiv [LabelView 38,CodeView 39 0],
      BorderDiv [LabelView 40,CodeView 41 0],
      BorderDiv [LabelView 42,CodeView 43 0],
      BorderDiv [LabelView 44,CodeView 45 0],
      BorderDiv [LabelView 46,CodeView 47 0],
      BorderDiv [LabelView 48,CodeView 49 0],
      BorderDiv [LabelView 50,CodeView 51 0],
      BorderDiv [LabelView 52,CodeView 53 0],
      BorderDiv [LabelView 54,CodeView 55 0]
      ]),

      ("fourbyeight", GridView 4 8  [
      (Views [LabelView 0,CodeView 1 0]),
       (Views [LabelView 2,CodeView 3 0]),
       (Views [LabelView 4,CodeView 5 0]),
       (Views [LabelView 6,CodeView 7 0]),
       (Views [LabelView 8,CodeView 9 0]),
       (Views [LabelView 10,CodeView 11 0]),
       (Views [LabelView 12,CodeView 13 0]),
       (Views [LabelView 14,CodeView 15 0]),
       (Views [LabelView 16,CodeView 17 0]),
       (Views [LabelView 18,CodeView 19 0]),
       (Views [LabelView 20,CodeView 21 0]),
       (Views [LabelView 22,CodeView 23 0]),
       (Views [LabelView 24,CodeView 25 0]),
       (Views [LabelView 26,CodeView 27 0]),
       (Views [LabelView 28,CodeView 29 0]),
       (Views [LabelView 30,CodeView 31 0]),
       (Views [LabelView 32,CodeView 33 0]),
       (Views [LabelView 34,CodeView 35 0]),
       (Views [LabelView 36,CodeView 37 0]),
       (Views [LabelView 38,CodeView 39 0]),
       (Views [LabelView 40,CodeView 41 0]),
       (Views [LabelView 42,CodeView 43 0]),
       (Views [LabelView 44,CodeView 45 0]),
       (Views [LabelView 46,CodeView 47 0]),
       (Views [LabelView 48,CodeView 49 0]),
       (Views [LabelView 50,CodeView 51 0]),
       (Views [LabelView 52,CodeView 53 0]),
       (Views [LabelView 54,CodeView 55 0]),
       (Views [LabelView 56,CodeView 57 0]),
       (Views [LabelView 58,CodeView 59 0]),
       (Views [LabelView 60,CodeView 61 0]),
       (Views [LabelView 62,CodeView 63 0])
      ]),

-- the second number of the RouletteView will modify the height of the view. It is expressed in ems, it is recommended to use numbers above 2 to avoid the roulette buttons being cutoff. If set to 0, the roulette view will expand automatically and the text will wrap.

      ("roulette",GridView 2 1 [
        BorderDiv [RouletteView 0 2, CodeView 1 0],
        BorderDiv [RouletteView 2 2, CodeView 3 0]
        ]),

      ("tempoAndCode", GridView 2 1  [
      BorderDiv [LabelView 0,CodeView 1 0],
      BorderDiv [SeeTimeView 0]
      ]),

      ("sandClockAndCode", GridView 2 1  [
      BorderDiv [LabelView 0,CodeView 1 0],
      BorderDiv [SandClockView 0]
      ]),

      ("countDownAndCode", GridView 2 1  [
      BorderDiv [LabelView 0,CodeView 1 0],
      BorderDiv [CountDownView 0]
      ]),

      ("stopWatchDownAndCode", GridView 2 1  [
      BorderDiv [LabelView 0,CodeView 1 0],
      BorderDiv [CountDownView 0]
      ])
      ]

      
