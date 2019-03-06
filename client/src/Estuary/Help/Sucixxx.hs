{-# LANGUAGE OverloadedStrings #-}
module Estuary.Help.Sucixxx where

import Reflex
import Reflex.Dom
import Data.Text
import GHCJS.DOM.EventM
import Estuary.Widgets.Generic

-- import Estuary.Types.Language

--render multiple sub-help files
sucixxxHelpFile :: MonadWidget t m => m ()
sucixxxHelpFile = divClass "languageHelp" $ do
  about
  functionRef "putita-"
  functionRef "perrita-"
  functionRef "tu sicaria-"
  functionRef "mala mujer-"
  functionRef "amorfada-"
  functionRef "gata fiera-"
  functionRef "torta golosa-"
  functionRef "feminasty-"
  functionRef "cómeme"
  functionRef "dame"
  functionRef "azótame"
  functionRef "rómpeme"
  functionRef "interpelame"
  functionRef "encadename"
  functionRef "aborta"
  functionRef "suave"
  functionRef "suave suavecito"
  functionRef "duro"
  functionRef "más más"
  functionRef "con lengua"
  functionRef "con el pelo"
  functionRef "bb"
  functionRef "con flow"
  return ()


-- about
about :: MonadWidget t m => m ()
about = do
 divClass "about" $ text "Sucixxx"
 divClass "about" $ text "A mini live coding esolang developed in Quito, Ecuador."

exampleText :: Text -> Text

exampleText "putita-" = "putita-comeme suave"
exampleText "perrita-" = "perrita-dame suave suavecito"
exampleText "tu sicaria-" = "tu sicaria-azótame duro"
exampleText "mala mujer-" =  "mala mujer-rómpeme más más"
exampleText "amorfada-" = "amorfada-interpelame con lengua"
exampleText "gata fiera-" = "gata fiera-aborta con el pelo"
exampleText "torta golosa-" = "torta golosa-cómeme bb"
exampleText "feminasty-" = "feminasty-dame con flow"
exampleText "cómeme" = "putita-comeme"
exampleText "dame" = "perrita-dame"
exampleText "azótame" = "tu sicaria-azótame"
exampleText "rómpeme" = "mala mujer-rómpeme"
exampleText "interpelame" = "amorfada-interpelame"
exampleText "encadename" = "gata fiera-encadename"
exampleText "aborta" = "gata fiera-aborta"
exampleText "suave" = "perrita-dame suave suavecito"
exampleText "suave suavecito" = "sueña las hojas volando"
exampleText "duro" = "tu sicaria-azótame duro"
exampleText "más más" =  "mala mujer-rómpeme más más"
exampleText "con lengua" = "amorfada-interpelame con lengua"
exampleText "con el pelo" = "gata fiera-aborta con el pelo"
exampleText "bb" ="torta golosa-cómeme bb"
exampleText "con flow" = "feminasty-dame con flow"


referenceText :: Text -> Text

referenceText "putita-" = "returns and empty string"
referenceText "perrita-" = "returns and empty string"
referenceText "tu sicaria-" = "returns and empty string"
referenceText "mala mujer-" =  "returns and empty string"
referenceText "amorfada-" = "returns and empty string"
referenceText "gata fiera-" = "returns and empty string"
referenceText "torta golosa-" = "returns and empty string"
referenceText "feminasty-" = "returns and empty string"
referenceText "cómeme" = "returns Dirt's \"fire\" sample"
referenceText "dame" = "returns Dirt's \"bass2\" sample"
referenceText "azótame" = "returns Dirt's \"808\" sample"
referenceText "rómpeme" = "returns Dirt's \"gabba\" sample"
referenceText "interpelame" = "returns Dirt's \"notes\" sample"
referenceText "encadename" = "returns Dirt's \"metal\" sample"
referenceText "aborta" = "returns Dirt's \"arpy\" sample"
referenceText "suave" = "returns TidalCycles' slow"
referenceText "suave suavecito" = "returns TidalCycles' slow"
referenceText "duro" = "returns TidalCycles'fast"
referenceText "más más" = "returns TidalCycles' fast"
referenceText "con lengua" = "returns TidalCycles' striate"
referenceText "con el pelo" = "returns TidalCycles' chop"
referenceText "bb" = "returns TidalCycles' reverb"
referenceText "con flow" = "returns TidalCycles' rev"

-- help files for samples
functionRef :: MonadWidget t m => Text -> m ()
functionRef x = divClass "helpWrapper" $ do
 switchToReference <- divClass "refExampleButton" $ button x
 exampleVisible <- toggle True switchToReference
 referenceVisible <- toggle False switchToReference
 hideableWidget exampleVisible "exampleText" $ text (exampleText x)
 hideableWidget referenceVisible "referenceText" $ text (referenceText x)
 return ()
