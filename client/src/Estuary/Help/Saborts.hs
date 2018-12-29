{-# LANGUAGE OverloadedStrings #-}
module Estuary.Help.Saborts where

import Reflex
import Reflex.Dom
import Data.Text
import GHCJS.DOM.EventM
import Estuary.Widgets.Generic

sabortsHelpFile :: MonadWidget t m => m ()
sabortsHelpFile = divClass "languageHelp" $ do
  about
  functionRef "g"
  functionRef "b"
  functionRef "v"
  functionRef "d"
  functionRef "k"
  functionRef "m"
  functionRef "h"
  functionRef "i"
  functionRef "t"
  functionRef "a"
  functionRef "c"
  functionRef "o"
  functionRef "e"
  functionRef "w"
  functionRef "q"
  functionRef "s"
  functionRef "z"
  return ()

  -- about
about :: MonadWidget t m => m ()
about = do
  divClass "about" $ text "Saborts"
  divClass "aboutText" $ text "A mini live coding esolang developed in Quito, Ecuador by RGGTRN."

exampleText :: Text -> Text

exampleText "g" = ":> g"
exampleText "b" = ":> b"
exampleText "v" = ":> v"
exampleText "d" = ":> d"
exampleText "k" = ":> k"
exampleText "m" = ":> m"
exampleText "h" = ":> h"
exampleText "i" = ":> i"
exampleText "t" = ":> t"
exampleText "a" = ":> a"
exampleText "c" = ":> c"
exampleText "o" = ":> o"
exampleText "e" = ":> e"
exampleText "w" = ":> gw"
exampleText "q" = ":> bq3"
exampleText "s" = ":> vs0.5"
exampleText "z" = ":> dz8"

referenceText :: Text -> Text

referenceText "g" = "returns Dirt's \"drumtraks\" sample"
referenceText "b" = "returns Dirt's \"bd\" sample"
referenceText "v" = "returns Dirt's \"bd:1\" sample"
referenceText "d" = "returns Dirt's \"bd:2\" sample"
referenceText "k" = "returns Dirt's \"bd:3\" sample"
referenceText "m" = "returns Dirt's \"bass:1\" sample"
referenceText "h" = "returns Dirt's \"hh27\" sample"
referenceText "i" = "returns Dirt's \"hh:7\" sample"
referenceText "t" = "returns Dirt's \"sn:1\" sample"
referenceText "a" = "returns Dirt's \"sn:2\" sample"
referenceText "c" = "returns Dirt's \"cp:1\" sample"
referenceText "o" = "returns Dirt's \"drum\" sample"
referenceText "e" = "returns Dirt's \"drum:1\" sample"
referenceText "w" =  "returns TidalCycles' brak"
referenceText "q" =  "returns TidalCycles' fast"
referenceText "s" =  "returns TidalCycles' slow"
referenceText "z" =  "returns TidalCycles' gap"


  -- help files for samples
functionRef :: MonadWidget t m => Text -> m ()
functionRef x = divClass "helpWrapper" $ do
  switchToReference <- divClass "refExampleButton" $ button x
  exampleVisible <- toggle True switchToReference
  referenceVisible <- toggle False switchToReference
  hideableWidget exampleVisible "exampleText" $ text (exampleText x)
  hideableWidget referenceVisible "referenceText" $ text (referenceText x)
  return ()
