{-# LANGUAGE OverloadedStrings #-}
module Estuary.Help.Vocesotrevez where

import Reflex
import Reflex.Dom
import Data.Text
import GHCJS.DOM.EventM
import Estuary.Widgets.Generic

--render multiple sub-help files
vocesotrevezHelpFile :: MonadWidget t m => m ()
vocesotrevezHelpFile = divClass "languageHelp" $ do
  about
  functionRef "otirg"
  functionRef "odimeg"
  functionRef "odiblis"
  functionRef "mmm"
  functionRef "uuu"
  functionRef ","
  functionRef ";"
  return ()


-- about
about :: MonadWidget t m => m ()
about = do
 divClass "about" $ text "Vocesotrevez"
 divClass "aboutText" $ text "A mini live coding esolang developed in Quito, Ecuador."

exampleText :: Text -> Text

exampleText "otirg" = "otirg"
exampleText "odimeg" = "odimeg"
exampleText "odiblis" = "odiblis"
exampleText "mmm" =  "otirg mmm 4"
exampleText "uuu" = "odiblis uuu"
exampleText "," = "odimeg , 2"
exampleText ";" = "odiblis ; 0.5"

referenceText :: Text -> Text

referenceText "otirg" = "returns Dirt's \"alphabet\" sample"
referenceText "odimeg" = "returns Dirt's \"moan\" sample"
referenceText "odiblis" = "returns Dirt's \"space\" sample"
referenceText "mmm" =  "returns TidalCycles' iter"
referenceText "uuu" = "returns TidalCycles' chop"
referenceText "," = "returns TidalCycles' fast"
referenceText ";" = "returns TidalCycles' slow"

-- help files for samples
functionRef :: MonadWidget t m => Text -> m ()
functionRef x = divClass "helpWrapper" $ do
 switchToReference <- divClass "refExampleButton" $ button x
 exampleVisible <- toggle True switchToReference
 referenceVisible <- toggle False switchToReference
 hideableWidget exampleVisible "exampleText" $ text (exampleText x)
 hideableWidget referenceVisible "referenceText" $ text (referenceText x)
 return ()
