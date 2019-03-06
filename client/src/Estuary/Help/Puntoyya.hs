{-# LANGUAGE OverloadedStrings #-}
module Estuary.Help.Puntoyya where

import Reflex
import Reflex.Dom
import Data.Text
import GHCJS.DOM.EventM
import Estuary.Widgets.Generic


--render multiple sub-help files
puntoyyaHelpFile :: MonadWidget t m => m ()
puntoyyaHelpFile = divClass "languageHelp" $ do
  about
  functionRef ". _ ."
  functionRef ". - ."
  functionRef ". | ."
  functionRef ". \" ."
  functionRef "o"
  functionRef "oo"
  functionRef "ooo"
  functionRef "oooo"
  return ()


-- about
about :: MonadWidget t m => m ()
about = do
 divClass "about" $ text "Punto y ya"
 divClass "about" $ text "A mini live coding esolang developed in Quito, Ecuador."

exampleText :: Text -> Text

exampleText ". _ ." = ". _ ."
exampleText ". - ." = ". - ."
exampleText ". \" ." = ". \" ."
exampleText ". | ." = ". | ."
exampleText "o" =  ". _ . o 2"
exampleText "oo" =  ". - . oo 0.5"
exampleText "ooo" = ". \". ooo 2"
exampleText "oooo" = ". - . oooo 2"

referenceText :: Text -> Text

referenceText ". _ ." = "returns Dirt's \"clap\" sample"
referenceText ". - ." = "returns Dirt's \"arpy\" sample"
referenceText ". \" ." = "returns Dirt's \"bass\" sample"
referenceText ". | ." = "returns Dirt's \"hh\" sample"
referenceText "o" =  "returns TidalCycles' fast "
referenceText "oo" = "returns TidalCycles' slow"
referenceText "ooo" = "returns TidalCycles' iter"
referenceText "oooo" = "returns TidalCycles' chop"

-- help files for samples
functionRef :: MonadWidget t m => Text -> m ()
functionRef x = divClass "helpWrapper" $ do
 switchToReference <- divClass "refExampleButton" $ button x
 exampleVisible <- toggle True switchToReference
 referenceVisible <- toggle False switchToReference
 hideableWidget exampleVisible "exampleText" $ text (exampleText x)
 hideableWidget referenceVisible "referenceText" $ text (referenceText x)
 return ()
