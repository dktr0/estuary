{-# LANGUAGE OverloadedStrings #-}
module Estuary.Help.Alobestia where

import Reflex
import Reflex.Dom
import Data.Text
import GHCJS.DOM.EventM
import Estuary.Widgets.Generic


--render multiple sub-help files
alobestiaHelpFile :: MonadWidget t m => m ()
alobestiaHelpFile = divClass "languageHelp" $ do
    about
    functionRef "boom"
    functionRef "clap"
    functionRef "zap"
    functionRef "rapido"
    functionRef "lento"
    functionRef "distorsion"
    functionRef "densidad"
    return ()


-- about
about :: MonadWidget t m => m ()
about = do
  divClass "about primary-color code-font" $ text "Alobestia reference"
  divClass "about primary-color code-font" $ text "A mini live coding esolang developed in Quito, Ecuador by Gaby Dávila y Lenin Moncayo."

exampleText :: Text -> Text

exampleText "boom" = "\"boom\""
exampleText "clap" = "\"zap\""
exampleText "zap" = "\"bass\""
exampleText "rapido" =  "\"boom\" rapido 2"
exampleText "lento" = "\"clap\" lento 2"
exampleText "distorsion" = "\"zap\" distorsion 2"
exampleText "densidad" = "\"zap\" densidad 4"

referenceText :: Text -> Text

referenceText "boom" = "returns Dirt's \"bd\" sample"
referenceText "clap" = "returns Dirt's \"cp\" sample"
referenceText "zap" = "returns Dirt's \"bass\" sample"
referenceText "rapido" = "returns TidalCycles' fast"
referenceText "lento" = "returns TidalCycles' slow"
referenceText "distorsion" = "returns TidalCycles' gap"
referenceText "densidad" = "returns TidalCycles' density"

-- help files for samples
functionRef :: MonadWidget t m => Text -> m ()
functionRef x = divClass "helpWrapper" $ do
   switchToReference <- divClass "" $ button x
   exampleVisible <- toggle True switchToReference
   referenceVisible <- toggle False switchToReference
   hideableWidget exampleVisible "exampleText primary-color code-font" $ text (exampleText x)
   hideableWidget referenceVisible "referenceText code-font" $ text (referenceText x)
   return ()
