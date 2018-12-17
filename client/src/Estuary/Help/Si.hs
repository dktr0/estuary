module Estuary.Help.Si where

import Reflex
import Reflex.Dom
import GHCJS.DOM.EventM
import Estuary.Widgets.Generic

-- import Estuary.Types.Language

--render multiple sub-help files
siHelpFile :: MonadWidget t m => m ()
siHelpFile = divClass "languageHelp" $ do
  about
  functionRef "Nose"
  functionRef "Willy"
  functionRef "Gracioso"
  functionRef "Elefante"
  functionRef "Pegado"
  functionRef "Lejos"
  functionRef "Tortuga"
  functionRef "Comadreja"
  return ()


  -- about
about :: MonadWidget t m => m ()
about = do
  divClass "about" $ text "Sentidos"
  divClass "aboutText" $ text "A mini live coding esolang developed in Bogotá, Colombia."

exampleText :: String -> String

exampleText "Nose" = "Nose"
exampleText "Willy" = "Willy"
exampleText "Gracioso" = "Gracioso"
exampleText "Elefante" = "Elefante"
exampleText "Pegado" =  "Willy Pegado "
exampleText "Lejos" =  "Nose lejos 4"
exampleText "Tortuga" =  "Gracioso Tortuga 0.5"
exampleText "Comadreja" =  "Elefante Comadreja 2"

referenceText :: String -> String

referenceText "Nose" = "returns Dirt's \"bd\" sample"
referenceText "Willy" = "returns Dirt's \"hh\" sample"
referenceText "Gracioso" = "returns Dirt's \"sn\" sample"
referenceText "Elefante" =  "returns Dirt's \"bass\" sample"
referenceText "Pegado" =  "returns TidalCycles' iter"
referenceText "Lejos" =  "returns TidalCycles' density"
referenceText "Tortuga" =  "returns TidalCycles' slow"
referenceText "Comadreja" =  "returns TidalCycles' fast"

  -- help files for samples
functionRef :: MonadWidget t m => String -> m ()
functionRef x = divClass "helpWrapper" $ do
  switchToReference <- divClass "refExampleButton" $ button x
  exampleVisible <- toggle True switchToReference
  referenceVisible <- toggle False switchToReference
  hideableWidget exampleVisible "exampleText" $ text (exampleText x)
  hideableWidget referenceVisible "referenceText" $ text (referenceText x)
  return ()