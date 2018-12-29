{-# LANGUAGE OverloadedStrings #-}
module Estuary.Help.CanvasOp where

import Reflex
import Reflex.Dom
import Data.Text
import GHCJS.DOM.EventM
import Estuary.Widgets.Generic

-- import Estuary.Types.Language

--render multiple sub-help files
canvasOpHelpFile :: MonadWidget t m => m ()
canvasOpHelpFile = divClass "languageHelp" $ do
    about
    functionRef "lineTo"
    functionRef "moveTo"
    functionRef "rect"
    functionRef "strokeStyle"
    return ()

-- about
about :: MonadWidget t m => m ()
about = do
  divClass "about" $ text "CanvasOp"
  divClass "aboutText" $ text "A mini language for building live-coding visual languages like PunctualVideo."

exampleText :: Text -> Text

exampleText "lineTo" = "lineTo 2 4"
exampleText "moveTo" = "moveTo 2 4"
exampleText "rect" = "rect 50 50 10 10"
exampleText "strokeStyle" =  "strokeStyle (100 25 40 100)"

referenceText :: Text -> Text

referenceText "lineTo" = "renders a line from a to b."
referenceText "moveTo" = "translates a shape from from a to b."
referenceText "rect" = "renders a rectangle using x <*> y <*> w <*> h values."
referenceText "strokeStyle" = "sets the color of the stroke in RGBA values"

-- help files for samples
functionRef :: MonadWidget t m => Text -> m ()
functionRef x = divClass "helpWrapper" $ do
   switchToReference <- divClass "refExampleButton" $ button x
   exampleVisible <- toggle True switchToReference
   referenceVisible <- toggle False switchToReference
   hideableWidget exampleVisible "exampleText" $ text (exampleText x)
   hideableWidget referenceVisible "referenceText" $ text (referenceText x)
   return ()
