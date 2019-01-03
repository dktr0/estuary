{-# LANGUAGE OverloadedStrings #-}
module Estuary.Help.Sentidos where

import Reflex
import Reflex.Dom
import Data.Text
import GHCJS.DOM.EventM
import Estuary.Widgets.Generic

--render multiple sub-help files
sentidosHelpFile :: MonadWidget t m => m ()
sentidosHelpFile = divClass "languageHelp" $ do
  about
  functionRef "rocoso"
  functionRef "melodioso"
  functionRef "ondulado"
  functionRef "agitado"
  functionRef "tristeza"
  functionRef "amor"
  return ()


  -- about
about :: MonadWidget t m => m ()
about = do
  divClass "about" $ text "Sentidos"
  divClass "aboutText" $ text "A mini live coding esolang developed in Bogotá, Colombia."

exampleText :: Text -> Text

exampleText "rocoso" = "rocoso"
exampleText "melodioso" = "melodioso"
exampleText "ondulado" = "ondulado"
exampleText "agitado" = "rocoso agitado 4"
exampleText "tristeza" =  "meliodioso tristeza 6"
exampleText "amor" =  "ondulado amor 3"

referenceText :: Text -> Text

referenceText "rocoso" = "returns Dirt's \"flick\" sample"
referenceText "melodioso" = "returns Dirt's \"sid\" sample"
referenceText "ondulado" = "returns Dirt's \"tabla\" sample"
referenceText "agitado" =  "returns TidalCycles' chop"
referenceText "tristeza" =  "returns TidalCycles' trunc"
referenceText "amor" =  "returns TidalCycles' iter"

  -- help files for samples
functionRef :: MonadWidget t m => Text -> m ()
functionRef x = divClass "helpWrapper" $ do
  switchToReference <- divClass "refExampleButton" $ button x
  exampleVisible <- toggle True switchToReference
  referenceVisible <- toggle False switchToReference
  hideableWidget exampleVisible "exampleText" $ text (exampleText x)
  hideableWidget referenceVisible "referenceText" $ text (referenceText x)
  return ()
