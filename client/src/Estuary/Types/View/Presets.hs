{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.View.Presets where

import Reflex.Dom
import Data.Map.Strict
import Data.Text
import Data.Bool (bool)
import qualified Data.Text as T


import Estuary.Types.View


genGrid :: Int -> Int -> Bool -> View
genGrid rows columns withBorder = GridView rows columns $ fmap v [0..(rows*columns-1)]
  where v n = bool Views BorderDiv withBorder [LabelView $ n*2, CodeView (n*2+1) 0 []]


presetViews :: Map Text View

-- note: if an ensemble publishes a view called 'default', that view rather than this one,
-- will effectively be the default view in that ensemble.


presetViews = fromList [

      -- ("def", genGrid 2 3 False),

      ("def",  GridView 2 2  [
      (Views [CodeView 1 0 ["fluxus"] ]),
      (Views [CodeView 2 0 [] ]),
      (Views [CodeView 3 0 [] ]),
      (Views [CodeView 4 0 ["fluxus"] ])
      ]),

      ("test",  GridView 2 3  [
      (Views [LabelView 1, CalendarEventView 2]),
      (Views [LabelView 3, CodeView 4 0 [] ]),
      (Views [LabelView 5, CodeView 6 0 [] ]),
      (Views [LabelView 7, CodeView 8 0 [] ]),
      (Views [LabelView 9, CodeView 10 0 [] ]),
      (Views [LabelView 11, CodeView 12 0 [] ])
      ]),

      ("fulltexteditor", genGrid 1 1 True),
      ("twocolumns", genGrid 2 1 True),
      ("twobyone", genGrid 2 1 True),
      ("twobytwo", genGrid 2 2 True),
      ("twobythree", genGrid 2 3 True),
      ("twobyfour", genGrid 2 4 True),
      ("twobyfive", genGrid 2 5 True),
      ("twobysix", genGrid 2 6 True),
      ("twobyseven", genGrid 2 7 True),
      ("twobyeight", genGrid 2 8 True),
      ("threebyone", genGrid 3 1 True),
      ("threebytwo", genGrid 3 2 True),
      ("threebythree", genGrid 3 3 True),
      ("threebyfour", genGrid 3 4 True),
      ("threebyfive", genGrid 3 5 True),
      ("threebysix", genGrid 3 6 True),
      ("threebyseven", genGrid 3 7 True),
      ("threebyeight", genGrid 3 8 True),
      ("fourbyone", genGrid 4 1 True),
      ("fourbytwo", genGrid 4 2 True),
      ("fourbythree", genGrid 4 3 True),
      ("fourbyfour", genGrid 4 4 True),
      ("fourbyfive", genGrid 4 5 True),
      ("fourbysix", genGrid 4 6 True),
      ("fourbyseven", genGrid 4 7 True),
      ("fourbyeight", genGrid 4 8 True),

-- the second number of the RouletteView will modify the height of the view. It is expressed in ems, it is recommended to use numbers above 2 to avoid the roulette buttons being cutoff. If set to 0, the roulette view will expand automatically and the text will wrap.

      ("roulette",GridView 2 1 [
        BorderDiv [RouletteView 0 2, CodeView 1 0 [] ],
        BorderDiv [RouletteView 2 2, CodeView 3 0 [] ]
        ]),

      ("tempoAndCode", GridView 2 1  [
      BorderDiv [LabelView 0,CodeView 1 0 []],
      BorderDiv [SeeTimeView 2]
      ]),

      ("sandClockAndCode", GridView 2 1  [
      BorderDiv [LabelView 0,CodeView 1 0 []],
      BorderDiv [SandClockView 2]
      ]),

      ("countDownAndCode", GridView 2 1  [
      BorderDiv [LabelView 0,CodeView 1 0 []],
      BorderDiv [CountDownView 2]
      ]),

      ("stopWatchDownAndCode", GridView 2 1  [
      BorderDiv [LabelView 0,CodeView 1 0 []],
      BorderDiv [CountDownView 2]
      ]),

      ("notepad", GridView 1 1 [
      BorderDiv [NotePadView 0]
      ]),

      ("notepadAndCode", GridView 1 2 [
      BorderDiv [NotePadView 0],
      BorderDiv [LabelView 0,CodeView 1 0 [] ]
      ]),

      ("notepadAndCodeWithCollapDiv", GridView 1 2 [
      Views [LabelView 1, CollapsableView (NotePadView 0), CodeView 2 0 [] ],
      Views [CodeView 3 0 [] ]
      ]),

      ("notepadAndCodeWithCollapDiv2", GridView 1 2 [
      CollapsableView (NotePadView 0),
      Views [LabelView 3,CodeView 4 0 [] ]
      ])

      ]
