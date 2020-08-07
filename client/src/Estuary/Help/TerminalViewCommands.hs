{-# LANGUAGE OverloadedStrings #-}

module Estuary.Help.TerminalViewCommands where

import Reflex
import Reflex.Dom
import Data.Text
import GHCJS.DOM.EventM
import Estuary.Widgets.Generic
import Estuary.Reflex.Utility

terminalViewCommandsHelpFile :: MonadWidget t m => m ()
terminalViewCommandsHelpFile = divClass "languageHelpContainer" $ divClass "languageHelp" $ do
  about
  functionRef "!localview"
  functionRef "!presetview"
  functionRef "!publishview"
  functionRef "!activeview"
  functionRef "!listviews"
  functionRef "!dumpview"
  return ()

about :: MonadWidget t m => m ()
about = do
  divClass "about primary-color code-font" $ text "A flexible view system is provided in order to create layouts for a wide range of purposes."

exampleText :: Text -> Text
exampleText "!localview" = "!localview 1x1 [border {[label:0, text:0 0]}]"
exampleText "!presetview" = "!presetview cybernetic"
exampleText "!publishview" = "!publishview basic"
exampleText "!activeview" =  "!activeview"
exampleText "!listviews" = "!listviews"
exampleText "!dumpview" = "!dumpview"

referenceText :: Text -> Text
referenceText "!localview" = "Creates a view"
referenceText "!presetview" = "Loads a view by name"
referenceText "!publishview" = "Publishes local view with a name"
referenceText "!activeview" =  "Returns the name of the current view"
referenceText "!listviews" = "Lists all preset views"
referenceText "!dumpview" = "Returns the current view's layout"

functionRef :: MonadWidget t m => Text -> m ()
functionRef x = divClass "helpWrapper" $ do
  switchToReference <- buttonWithClass' x
  exampleVisible <- toggle True switchToReference
  referenceVisible <- toggle False switchToReference
  hideableWidget exampleVisible "exampleText primary-color code-font" $ text (exampleText x)
  hideableWidget referenceVisible "referenceText code-font" $ text (referenceText x)
  return ()
