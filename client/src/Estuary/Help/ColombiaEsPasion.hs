{-# LANGUAGE OverloadedStrings #-}
module Estuary.Help.ColombiaEsPasion where

import Reflex
import Reflex.Dom
import Data.Text
import GHCJS.DOM.EventM
import Estuary.Widgets.Generic
import Estuary.Reflex.Utility

--render multiple sub-help files
colombiaEsPasionHelpFile :: MonadWidget t m => m ()
colombiaEsPasionHelpFile = divClass "languageHelp" $ do
    about
    functionRef "voz"
    functionRef "pasión"
    functionRef "paz"
    functionRef "educación"
    functionRef "protesta"
    functionRef "soacha"
    return ()


-- about
about :: MonadWidget t m => m ()
about = do
  divClass "about primary-color code-font" $ text "Colombia Es Pasion"
  divClass "about primary-color code-font" $ text "A mini live coding esolang developed in Bogotá, Colombia."

exampleText :: Text -> Text

exampleText "voz" = "\"voz voz\""
exampleText "pasión" = "\"pasión\""
exampleText "paz" = "\"paz paz paz\""
exampleText "educación" =  "\"pasión pasión pasión pasión\" educación 2"
exampleText "protesta" = "\"paz\" protesta 3"
exampleText "soacha" = "\"voz voz voz voz\" soacha"

referenceText :: Text -> Text

referenceText "voz" = "returns Dirt's \"birds3\" sample"
referenceText "pasión" = "returns Dirt's \"blip\" sample"
referenceText "paz" = "returns Dirt's \"sax\" sample"
referenceText "educación" = "returns TidalCycles' slow"
referenceText "protesta" = "returns TidalCycles' fast"
referenceText "soacha" = "returns TidalCycles' brak"

-- help files for samples
functionRef :: MonadWidget t m => Text -> m ()
functionRef x = divClass "helpWrapper" $ do
   switchToReference <- buttonWithClass' x
   exampleVisible <- toggle True switchToReference
   referenceVisible <- toggle False switchToReference
   hideableWidget exampleVisible "exampleText primary-color code-font" $ text (exampleText x)
   hideableWidget referenceVisible "referenceText code-font" $ text (referenceText x)
   return ()
