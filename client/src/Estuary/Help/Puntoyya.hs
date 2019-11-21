{-# LANGUAGE OverloadedStrings #-}
module Estuary.Help.Puntoyya where

import Reflex
import Reflex.Dom
import Data.Text
import GHCJS.DOM.EventM
import Estuary.Widgets.Generic
import Estuary.Reflex.Utility


--render multiple sub-help files
puntoyyaHelpFile :: MonadWidget t m => m ()
puntoyyaHelpFile = divClass "languageHelpContainer" $ divClass "languageHelp" $ do
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
 divClass "about primary-color code-font" $ text "Punto y ya"
 divClass "about primary-color code-font" $ text "A mini live coding esolang developed in Quito, Ecuador."

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
 switchToReference <- buttonWithClass' x
 exampleVisible <- toggle True switchToReference
 referenceVisible <- toggle False switchToReference
 hideableWidget exampleVisible "exampleText primary-color code-font" $ text (exampleText x)
 hideableWidget referenceVisible "referenceText code-font" $ text (referenceText x)
 return ()
