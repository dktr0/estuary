{-# LANGUAGE OverloadedStrings #-}

module Estuary.Tutorials.TidalCyclesBasics (tidalCyclesBasics) where

import Reflex.Dom
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Sequence as Seq

import Estuary.Types.Tutorial
import Estuary.Types.View
import Estuary.Types.TranslatableText
import Estuary.Types.Language
import Estuary.Types.Definition
import Estuary.Types.TextNotation
import Estuary.Types.TidalParser


tidalCyclesBasics ::  Tutorial
tidalCyclesBasics = Tutorial {
  tutorialTitle =  Map.fromList [
    (English, "TidalCycles basics")
    ],
  tutorialPages = Seq.fromList [
    TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"First Steps")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
          Paragraph [ Text $ Map.fromList [
            (English,"This tutorial will cover some of the basics of making music with MiniTidal. MiniTidal is a subset of TidalCycles that supports most typical TidalCycles operations (but not all), and everything shown here (and anything that works with MiniTidal) is easy to transfer to the standalone TidalCycles. Lets make some sound!")
            ]],
          Paragraph [ Text $ Map.fromList [
            (English,"Copy the code example below to the text editing area at the bottom of this page and then click Eval to \"evaluate\" it, or, if you prefer, click on the code example to automatically copy and evaluate it.")
            ]],
          Snippet 1 True (TidalTextNotation MiniTidal) "s \"bd cp\"",
          Paragraph [ Text $ Map.fromList [
            (English, "In the text field above, we've specified a pattern of samples (specifically 'bd' and 'cp') that fill up a 'cycle' (which can be thought of sort of like musical bars if you like). Everything that appears within the quotes (\"\") divides a cycle into equal parts: the \"bd\" sample gets the first half of the cycle, and the \"cp\" gets the second half. If we put a third element into our pattern we get a different rhythm - each sample gets 1/3rd of a cycle:")
            ]],
          Snippet 1 True (TidalTextNotation MiniTidal) "s \"bd cp sn\"",
          Paragraph [ Text $ Map.fromList [
            (English,"A \"~\" placed in a Tidal pattern has a special designation as a 'rest' (silence). So we can get rid of the 'sn' in the above example but preserve the rhythm:")
            ]],
          Snippet 1 True (TidalTextNotation MiniTidal) "s \"bd cp ~\"",
          Paragraph [ Text $ Map.fromList [
            (English,"You can try to make your own patterns by adding more names of samples (or more ~ for silences) between the quotation marks. When you're ready for more, click next above to go to the next page of this tutorial.")
            ]]
          ],
        CodeView 1 0
      ]
    },
    TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Patterns within Patterns")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
          Paragraph [ Text $ Map.fromList [
            (English,"Sometimes we want to subdivide a part of a cycle - for instance if we want 2 samples to play in the last half of a cycle instead of just one: ")
            ]],
          Snippet 1 True (TidalTextNotation MiniTidal) "s \"bd [cp sn]\"",
          Paragraph [ Text $ Map.fromList [
            (English,"Or if we want two samples to play at the same time we can enclose them in square brackets with a comma between them: ")
            ]],
          Snippet 1 True (TidalTextNotation MiniTidal) "s \"bd [cp,sn]\"",
          Paragraph [ Text $ Map.fromList [
            (English,"Both ideas together: ")
            ]],
          Snippet 1 True (TidalTextNotation MiniTidal) "s \"bd [cp,sn casio]\""
          ],
        CodeView 1 0
      ]
    },
    TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"More Fun Stuff with Tidal")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
          Paragraph [ Text $ Map.fromList [
            (English,"Here are various additional examples to tinker with (without explanation yet, sorry!). You can find more examples of Tidal(Cycles) usage at tidalcycles.org (just note that the examples there all have things like \"d1 $\" in front of them - in MiniTidal/Estuary you need to leave off that initial d1 $ in the examples you see elsewhere):")
            ]],
          Snippet 1 True (TidalTextNotation MiniTidal) "fast 2 $ s \"bd cp\"",
          Snippet 1 True (TidalTextNotation MiniTidal) "jux (fast 2) $ s \"bd cp\"",
          Snippet 1 True (TidalTextNotation MiniTidal) "s \"bd? sn? hh? cp?\"",
          Snippet 1 True (TidalTextNotation MiniTidal) "every 4 (fast 4) $ s \"bd cp\"",
          Snippet 1 True (TidalTextNotation MiniTidal) "s \"arpy*8\" # note \"0 2 4 5 7 9 11 12\"",
          Snippet 1 True (TidalTextNotation MiniTidal) "s \"glitch\" # n (irand 8)",
          Snippet 1 True (TidalTextNotation MiniTidal) "stack [s \"bd cp\",s \"arpy*4\" # note \"[0,4,7]\" ] "
          ],
        CodeView 1 0
      ]
    }
  ]
}
