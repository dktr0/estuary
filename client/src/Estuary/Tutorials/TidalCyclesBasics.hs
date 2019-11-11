{-# LANGUAGE OverloadedStrings #-}

module Estuary.Tutorials.TidalCyclesBasics (tidalCyclesBasics) where

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

tidalCyclesBasics :: Tutorial
tidalCyclesBasics = Tutorial {
  tutorialTitle = Map.fromList [
    (English,"TidalCycles basics")
  ],
  tutorialPages = Seq.fromList [
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Welcome!")
      ],
      tutorialPageView = Views [
        Paragraph $ Map.fromList [(English,"This tutorial will cover some of the basics of making music with MiniTidal. MiniTidal is a subset of TidalCycles that supports most typical TidalCycles operations (but not all), but everything shown here (and anything that works with MiniTidal) will also work with TidalCycles. Lets make some sound!")],
        Paragraph $ Map.fromList [(English,"Copy the code below to the text editing area below and then click Eval to \"evaluate\" it, or, if you prefer, click on the code example to automatically copy and evaluate it.")],
        Example (TidalTextNotation MiniTidal) "s \"bd cp\"",
        TextView 1 5
      ]
    }),
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Page Two!")
      ],
      tutorialPageView = Views [
        Paragraph $ Map.fromList [
          (English,"Here is a second page of a tutorial."),
          (Español,"Aqui esta la segunda pagina."),
          (Français,"Ici se trouve la deuxieme page.")],
        Example (TidalTextNotation MiniTidal) "s \"arpy*8\"",
        TextView 1 5
      ]
    })

  ]
}

{-
  el "div" $ labelWidget ctx $ fromList [(English, "In the text field above, we've specified a pattern of samples (specifically 'bd' and 'cp') that fill up a 'cycle' (which can be thought of sort of like musical bars if you like). Everything that appears within the quotes (\"\") divides a cycle into equal parts: the \"bd\" sample gets the first half of the cycle, and the \"cp\" gets the second half. ")]

  el "div" $ labelWidget ctx $ fromList [(English,"If we put a third element into our pattern we get a different rhythm:")]

  (v2,h2) <- miniTidalWidget ctx 1 2 "s \"bd cp hh\""

  el "div" $ labelWidget ctx $ fromList [(English,"Each sample gets 1/3rd of a cycle.")]

  el "div" $ labelWidget ctx $ fromList [(English,"A \"~\" placed in a Tidal pattern has a special designation as a 'rest'. So we can get rid of the 'hh' in the above example but preserve the rhythm:")]
  (v3,h3) <- miniTidalWidget ctx 1 3 "s \"bd cp ~\""

  el "div" $ labelWidget ctx $ fromList [(English,"Sometimes we want to subdivide a part of a cycle - for instance if we want 2 samples to play in the last half of a cycle instead of just one: ")]
  (v4, h4) <- miniTidalWidget ctx 1 4 "s \"bd [cp hh]\""

  el "div" $ labelWidget ctx $ fromList [(English,"Or if we want two samples to play at the same time we can enclose them in square brackets with a comma between them: ")]
  (v5, h5) <- miniTidalWidget ctx 1 5 "s \"bd [cp,hh]\""

  el "div" $ labelWidget ctx $ fromList [(English,"Both ideas together: ")]
  (v6, h6) <- miniTidalWidget ctx 1 5 "s \"bd [cp,hh casio]\""

  el "div" $ labelWidget ctx $ fromList [(English,"")]
-}
