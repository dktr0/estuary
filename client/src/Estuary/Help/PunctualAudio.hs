{-# LANGUAGE OverloadedStrings #-}
module Estuary.Help.PunctualAudio where

import Reflex
import Reflex.Dom
import Data.Text
import GHCJS.DOM.EventM
import Estuary.Widgets.Generic

-- import Estuary.Types.Language

--render multiple sub-help files
punctualAudio :: MonadWidget t m => m ()
punctualAudio = divClass "languageHelp" $ do
    about
    sine
    saw
    tri
    return ()

-- about
about :: MonadWidget t m => m ()
about = do
  divClass "about" $ text "PunctualAudio"
  divClass "aboutText" $ text "A mini language for synthesis in the browser."

-- help files for samples
sine :: MonadWidget t m => m ()
sine = divClass "helpWrapper" $ do
   switchToReference <- divClass "refExampleButton" $ button "sin:"
   exampleVisible <- toggle True switchToReference
   referenceVisible <- toggle False switchToReference
   hideableWidget exampleVisible "exampleText" $ text "sin 60m * -20db" --languageHelpWidget MiniTidal
   hideableWidget referenceVisible "referenceText" $ text "Generates a sine wave." --languageHelpWidget MiniTidal
   return ()

   -- help files for samples
saw :: MonadWidget t m => m ()
saw = divClass "helpWrapper" $ do
   switchToReference <- divClass "refExampleButton" $ button "saw:"
   exampleVisible <- toggle True switchToReference
   referenceVisible <- toggle False switchToReference
   hideableWidget exampleVisible "exampleText" $ text "saw 67m * -20db"
   hideableWidget referenceVisible "referenceText" $ text "Generates a saw wave." --languageHelpWidget MiniTidal
   return ()


   -- help files for samples
tri :: MonadWidget t m => m ()
tri = divClass "helpWrapper" $ do
   switchToReference <- divClass "refExampleButton" $ button "tri:"
   exampleVisible <- toggle True switchToReference
   referenceVisible <- toggle False switchToReference
   hideableWidget exampleVisible "exampleText" $ text "tri 62 * -20db"
   hideableWidget referenceVisible "referenceText" $ text "generates a triangle wave."
   return ()
