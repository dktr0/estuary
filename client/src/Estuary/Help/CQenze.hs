{-# LANGUAGE OverloadedStrings #-}
module Estuary.Help.CQenze where

import Reflex
import Reflex.Dom
import Data.Text
import GHCJS.DOM.EventM
import Estuary.Widgets.Generic

--render multiple sub-help files
cqenzeHelpFile :: MonadWidget t m => m ()
cqenzeHelpFile = divClass "languageHelp" $ do
  about
  functionRef "+"
  functionRef "-"
  functionRef "?"
  functionRef "f"
  functionRef "s"
  functionRef "r"
  functionRef "b"
  functionRef "c"
  return ()

  -- about
about :: MonadWidget t m => m ()
about = do
    divClass "about" $ text "CQenze"
    divClass "about" $ text "A mini live-coding language based on the ChucK based CQenze by Estaban Betancur."

exampleText :: Text -> Text

exampleText "+" = "industrial++++"
exampleText "-" = "industrial+-+-"
exampleText "?" = "industrial++++?"
exampleText "f" =  "drum+-+-f"
exampleText "s" = "drum+-+-s"
exampleText "r" = "bev++++r"
exampleText "b" = "bev++++b"
exampleText "c" = "bass++++c"

referenceText :: Text -> Text
referenceText "+" = "returns a beat"
referenceText "-" = "returns a silence"
referenceText "?" = "returns TidalCycles' degrade"
referenceText "f" = "returns TidalCycles' fast 2"
referenceText "s" = "returns TidalCycles' slow 2"
referenceText "r" = "returns TidalCycles' rev"
referenceText "b" = "returns TidalCycles' brak"
referenceText "c" = "returns TidalCycles' chop 2"

  -- help files for samples
functionRef :: MonadWidget t m => Text -> m ()
functionRef x = divClass "helpWrapper" $ do
 switchToReference <- divClass "refExampleButton" $ button x
 exampleVisible <- toggle True switchToReference
 referenceVisible <- toggle False switchToReference
 hideableWidget exampleVisible "exampleText" $ text (exampleText x)
 hideableWidget referenceVisible "referenceText" $ text (referenceText x)
 return ()
