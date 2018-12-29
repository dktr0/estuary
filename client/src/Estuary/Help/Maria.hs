{-# LANGUAGE OverloadedStrings #-}
module Estuary.Help.Maria where

import Reflex
import Reflex.Dom
import Data.Text
import GHCJS.DOM.EventM
import Estuary.Widgets.Generic

--render multiple sub-help files
mariaHelpFile :: MonadWidget t m => m ()
mariaHelpFile = divClass "languageHelp" $ do
  about
  functionRef "María"
  functionRef "toca"
  functionRef "abre"
  functionRef "cierra"
  functionRef "rápido"
  functionRef "lento"
  functionRef "puerta"
  functionRef "suave"
  functionRef "ventana"
  functionRef "botella"
  return ()

-- about
about :: MonadWidget t m => m ()
about = do
 divClass "about" $ text "María"
 divClass "aboutText" $ text "A mini live coding esolang developed in Quito, Ecuador."

exampleText :: Text -> Text

exampleText "María" = "María toca"
exampleText "toca" = "María toca"
exampleText "abre" = "María abre"
exampleText "cierra" = "María cierra"
exampleText "tapa" = "María tapa"
exampleText "rápido" =  "María toca rápido 2"
exampleText "lento" = "María abre lento 0.5"
exampleText "puerta" = "María abre la puerta 2"
exampleText "suave" = "María toca suave"
exampleText "ventana" = "María abre la ventana 8"
exampleText "botella" = "María cierra la botella 4"

referenceText :: Text -> Text

referenceText "María" = "Parses the begining of the document"
referenceText "toca" = "returns Dirt's \"bd\" sample"
referenceText "abre" = "returns Dirt's \"arpy\" sample"
referenceText "cierra" = "returns Dirt's \"cp\" sample"
referenceText "tapa" = "returns Dirt's \"bass\" sample"
referenceText "rápido" = "returns TidalCycles' fast"
referenceText "lento" = "returns TidalCycles' slow"
referenceText "puerta" = "returns TidalCycles' density"
referenceText "suave" = "returns TidalCycles' chop"
referenceText "ventana" = "returns TidalCycles' striate"
referenceText "botella" = "returns TidalCycles' iter"

functionRef :: MonadWidget t m => Text -> m ()
functionRef x = divClass "helpWrapper" $ do
 switchToReference <- divClass "refExampleButton" $ button x
 exampleVisible <- toggle True switchToReference
 referenceVisible <- toggle False switchToReference
 hideableWidget exampleVisible "exampleText" $ text (exampleText x)
 hideableWidget referenceVisible "referenceText" $ text (referenceText x)
 return ()
