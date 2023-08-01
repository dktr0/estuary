{-# LANGUAGE OverloadedStrings #-}

module Estuary.Tutorials.Metre (metreTutorial) where

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

metreTutorial :: Tutorial
metreTutorial = Tutorial {
  tutorialTitle = Map.fromList [
    (English,"Metre tutorial"),
    (Español,"Tutorial del Compás")
  ],
  tutorialPages = Seq.fromList [
    -- ///////////////////////////////////////////////////////////////
    --page 1: tutorial 
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
      (English,"Metre Tutorial"),
      (Español,"Tutorial del Compás")
      ],
      tutorialPageView = GridView 1 2 [
           Views [
          
              Paragraph [ Text $ Map.fromList[
                (English,"The Metre (or Meter) widget will allow players to visualise Estuary’s tempo in contrast with metric/rhythmic subdivisions and Bjorklund patterns. This is useful for pedagogical purposes; for example, to show how a pattern relates with the elapsing cycles or to explain how Bjorklund patterns work, but it can also be used to facilitate synchronisation between instrumentalists and an Estuary ensemble."),
                (Español,"")
              ]],
              Paragraph [ Text $ Map.fromList[
                (English,"You can call the Metre/Meter with the command in the View system ‘metre Int’ or ‘meter Int’. For example, ‘metre 10’ will generate a metre widget that occupies the index 10 in the view system. The metre visualiser has an interface based on clickable areas; if you hoover over it you will get indications of what would happen if you click on that area. As you can see, the right will take you to the next visualiser, the left to the previous one, up in the middle will add a new subdivision, and the one down in the middle will substract a subdivision."),
                (Español,"")
              ]],
              Paragraph [ Text $ Map.fromList[
                (English, "To begin, activate the timer (by pressing the reset or resume/pause areas) and change the visualisation mode. Observe the different ways in which we are representing ellapsing time. Some have labels for each section, some are more visual, some display numbers and text. The visualisers are: progress bar with labels, progress bar, sand-clock with labels, sand-clock, numeric with label, numeric, only labels, circular, and stack."),
                (Español,"")
              ]],
              Paragraph [ Text $ Map.fromList[
                (English, "The visualisers are divided into 2 categories expensive (in terms of computational resources) and (relatively) cheap. The visualisers are: circular expensive, circular cheap, squared expensive, squared cheap, bead expensive and bead cheap. The bead visualisers present a special behaviour: the middle area is divided into three (up, centre, down) instead of two (up and down). The additional central area will add a ‘k’ value to form a Euclidean pattern on top of the subdivided metre, which will act as the ‘m’. To learn more about these musical patterns see Toussaint's paper: 'The Euclidean Algorithm Generates Traditional Musical Rhythms'"
                ),
                (Español,"")
              ]]
              ],
          MetreView 0
      ]
    })
  ]
}
