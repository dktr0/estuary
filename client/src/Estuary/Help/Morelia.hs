{-# LANGUAGE OverloadedStrings #-}
module Estuary.Help.Morelia where

import Reflex
import Reflex.Dom
import Data.Text
import GHCJS.DOM.EventM
import Estuary.Widgets.Generic

-- import Estuary.Types.Language

--render multiple sub-help files
moreliaHelpFile :: MonadWidget t m => m ()
moreliaHelpFile = divClass "languageHelp" $ do
  about
  functionRef "~"
  functionRef "X"
  functionRef "IX"
  functionRef "VIII"
  functionRef "VII"
  functionRef "VI"
  functionRef "V"
  functionRef "IV"
  functionRef "III"
  functionRef "II"
  functionRef "I"
  return ()

-- about
about :: MonadWidget t m => m ()
about = do
 divClass "about" $ text "Morelia"
 divClass "about" $ text "A mini live coding esolang developed in Morelia, México."

exampleText :: Text -> Text

exampleText "~" = "\"X\" ~"
exampleText "X" = "\"X X X X\" ~"
exampleText "IX" = "\"IX IX\" ~"
exampleText "VIII" = "\"VIII\" ~"
exampleText "VII" = "\"VII VII VII\" ~"
exampleText "VI" =  "\"VI VI\" ~"
exampleText "V" = "\"V\" ~"
exampleText "IV" = "\"IV IV IV IV\" ~"
exampleText "III" = "\"III III III\" ~"
exampleText "II" = "\"II II\" ~"
exampleText "I" = "\"I\" ~"

referenceText :: Text -> Text

referenceText "~" = "parses the end of document"
referenceText "X" = "returns Dirt's \"sugar\" sample"
referenceText "IX" = "returns Dirt's \"able\" sample"
referenceText "VIII" = "returns Dirt's \"peri\" sample"
referenceText "VII" = "returns Dirt's \"jazz\" sample"
referenceText "VI" = "returns Dirt's \"moan\" sample"
referenceText "V" = "returns Dirt's \"kurt\" sample"
referenceText "IV" = "returns Dirt's \"feelfx\" sample"
referenceText "III" = "returns Dirt's \"hh\" sample"
referenceText "II" = "returns Dirt's \"sn\" sample"
referenceText "I" = "returns Dirt's \"bd\" sample"

functionRef :: MonadWidget t m => Text -> m ()
functionRef x = divClass "helpWrapper" $ do
 switchToReference <- divClass "refExampleButton" $ button x
 exampleVisible <- toggle True switchToReference
 referenceVisible <- toggle False switchToReference
 hideableWidget exampleVisible "exampleText" $ text (exampleText x)
 hideableWidget referenceVisible "referenceText" $ text (referenceText x)
 return ()
