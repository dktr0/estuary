{-# LANGUAGE OverloadedStrings #-}
module Estuary.Help.Medellin where

import Reflex
import Reflex.Dom
import Data.Text
import GHCJS.DOM.EventM
import Estuary.Widgets.Generic
import Estuary.Reflex.Utility


--render multiple sub-help files
medellinHelpFile :: MonadWidget t m => m ()
medellinHelpFile = divClass "languageHelp" $ do
  about
  functionRef "bombo"
  functionRef "maraca"
  functionRef "fast"
  functionRef "density"
  return ()


-- about
about :: MonadWidget t m => m ()
about = do
 divClass "about primary-color code-font" $ text "Medellin"
 divClass "about primary-color code-font" $ text "A mini live coding esolang developed in Medellin, Colombia."

exampleText :: Text -> Text

exampleText "bombo" = "bombo"
exampleText "maraca" = "maraca"
exampleText "fast" = "bombo fast 2"
exampleText "density" =  "maraca density 3"

referenceText :: Text -> Text

referenceText "bombo" = "returns Dirt's \"bd\" sample"
referenceText "maraca" = "returns Dirt's \"crow\" sample"
referenceText "fast" = "returns TidalCycles' fast"
referenceText "density" = "returns TidalCycles' density"

-- help files for samples
functionRef :: MonadWidget t m => Text -> m ()
functionRef x = divClass "helpWrapper" $ do
 switchToReference <- buttonWithClass' x
 exampleVisible <- toggle True switchToReference
 referenceVisible <- toggle False switchToReference
 hideableWidget exampleVisible "exampleText primary-color code-font" $ text (exampleText x)
 hideableWidget referenceVisible "referenceText code-font" $ text (referenceText x)
 return ()
