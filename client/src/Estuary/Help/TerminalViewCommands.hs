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
exampleText "!localview" = "!localview"
exampleText "!presetview" = "!presetview"
exampleText "!publishview" = "!publishview"
exampleText "!activeview" =  "!activeview"
exampleText "!listviews" = "!listviews"
exampleText "!dumpview" = "!dumpview"

referenceText :: Text -> Text
referenceText "!localview" = "Create a view"
referenceText "!presetview" = "Load a view"
referenceText "!publishview" = "Publish local view"
referenceText "!activeview" =  "Active a view"
referenceText "!listviews" = "List preset views"
referenceText "!dumpview" = "Dump views"

functionRef :: MonadWidget t m => Text -> m ()
functionRef x = divClass "helpWrapper" $ do
  switchToReference <- buttonWithClass' x
  exampleVisible <- toggle True switchToReference
  referenceVisible <- toggle False switchToReference
  hideableWidget exampleVisible "exampleText primary-color code-font" $ text (exampleText x)
  hideableWidget referenceVisible "referenceText code-font" $ text (referenceText x)
  return ()
