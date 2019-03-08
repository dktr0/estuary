{-# LANGUAGE OverloadedStrings #-}
module Estuary.Help.Imagina where

import Reflex
import Reflex.Dom
import Data.Text
import GHCJS.DOM.EventM
import Estuary.Widgets.Generic


--render multiple sub-help files
imaginaHelpFile :: MonadWidget t m => m ()
imaginaHelpFile = divClass "languageHelp" $ do
  about
  functionRef "imagina"
  functionRef "sueña"
  functionRef "mechita"
  functionRef "el agua"
  functionRef "las hojas"
  functionRef "el pájaro"
  functionRef "del río"
  functionRef "cayendo"
  functionRef "del mar"
  functionRef "del árbol"
  functionRef "caer"
  functionRef "crecer"
  functionRef "cantando"
  functionRef "comiendo"
  functionRef "volando"
  return ()


-- about
about :: MonadWidget t m => m ()
about = do
 divClass "about" $ text "imagina"
 divClass "about" $ text "A mini live coding esolang developed in Quito, Ecuador."

exampleText :: Text -> Text

exampleText "imagina" = "imagina el agua"
exampleText "sueña" = "sueña las hojas"
exampleText "mechita" = "mechita el agua"
exampleText "el agua" =  "sueña el agua"
exampleText "las hojas" = "imagina las hojas"
exampleText "el pájaro" = "mechita el pájaro"
exampleText "del río" = "imagina el agua del rio"
exampleText "cayendo" = "sueña las hojas cayendo 2"
exampleText "del mar" = "imagina el agua del mar 4"
exampleText "del árbol" = "imagina las hojas del arbol"
exampleText "caer" = "sueña el pájaro caer 4"
exampleText "crecer" = "imagina las hojas crecer 3"
exampleText "cantando" = "sueña el pájaro cantando"
exampleText "comiendo" = "imagina el pájaro comiendo 0.2"
exampleText "volando" = "sueña las hojas volando 4"

referenceText :: Text -> Text

referenceText "imagina" = "returns and empty Text"
referenceText "sueña" = "returns and empty Text"
referenceText "mechita" = "returns and empty Text"
referenceText "el agua" =  "returns Dirt's \"pluck:3\" sample"
referenceText "las hojas" = "returns Dirt's \"wind\" sample"
referenceText "el pájaro" = "returns Dirt's \"birds3\" sample"
referenceText "del río" = "returns TidalCycles' palindrome"
referenceText "cayendo" = "returns TidalCycles' slow"
referenceText "del mar" = "returns TidalCycles' density"
referenceText "del árbol" = "returns TidalCycles' palindrome"
referenceText "caer" = "returns TidalCycles' slow"
referenceText "crecer" = "returns TidalCycles' density"
referenceText "cantando" = "returns TidalCycles' fast"
referenceText "volando" = "returns TidalCycles' density"
referenceText "comiendo" = "returns TidalCycles' trunc"


-- help files for samples
functionRef :: MonadWidget t m => Text -> m ()
functionRef x = divClass "helpWrapper" $ do
 switchToReference <- divClass "refExampleButton" $ button x
 exampleVisible <- toggle True switchToReference
 referenceVisible <- toggle False switchToReference
 hideableWidget exampleVisible "exampleText" $ text (exampleText x)
 hideableWidget referenceVisible "referenceText" $ text (referenceText x)
 return ()
