{-# LANGUAGE OverloadedStrings #-}
module Estuary.Help.PunctualVideo where

import Reflex
import Reflex.Dom
import Data.Text
import GHCJS.DOM.EventM
import Estuary.Widgets.Generic

-- import Estuary.Types.Language

--render multiple sub-help files
punctualVideo :: MonadWidget t m => m ()
punctualVideo = divClass "languageHelp" $ do
    about
    clear
    sine
    saw
    tri
    return ()

-- about
about :: MonadWidget t m => m ()
about = do
  divClass "about" $ text "PunctualVideo"
  divClass "aboutText" $ text "A mini language for synthesis in the browser."

-- help files for samples
clear :: MonadWidget t m => m ()
clear = divClass "helpWrapper" $ do
   switchToReference <- divClass "refExampleButton" $ button "clear:"
   exampleVisible <- toggle True switchToReference
   referenceVisible <- toggle False switchToReference
   hideableWidget exampleVisible "exampleText" $ text "clear"
   hideableWidget referenceVisible "referenceText" $ text "Clears the visuals from the display" --languageHelpWidget MiniTidal
   return ()

sine :: MonadWidget t m => m ()
sine = divClass "helpWrapper" $ do
   switchToReference <- divClass "refExampleButton" $ button "sin:"
   exampleVisible <- toggle True switchToReference
   referenceVisible <- toggle False switchToReference
   hideableWidget exampleVisible "exampleText" $ text "x <> sin 0.9; y <> sin 0.4; w <> 0; h <> 0; clear <> 0.34"
   hideableWidget referenceVisible "referenceText" $ text "Generates a sine wave." --languageHelpWidget MiniTidal
   return ()

   -- help files for samples
saw :: MonadWidget t m => m ()
saw = divClass "helpWrapper" $ do
   switchToReference <- divClass "refExampleButton" $ button "saw:"
   exampleVisible <- toggle True switchToReference
   referenceVisible <- toggle False switchToReference
   hideableWidget exampleVisible "exampleText" $ text "x <> -1; y <> -1; w <>  saw 30m ; h <> saw 35m ; clear <> 0.14"
   hideableWidget referenceVisible "referenceText" $ text "Generates a saw wave." --languageHelpWidget MiniTidal
   return ()


   -- help files for samples
tri :: MonadWidget t m => m ()
tri = divClass "helpWrapper" $ do
   switchToReference <- divClass "refExampleButton" $ button "tri:"
   exampleVisible <- toggle True switchToReference
   referenceVisible <- toggle False switchToReference
   hideableWidget exampleVisible "exampleText" $ text "x <> tri 6; y <> tri 3; w <> -0.5 ; h <> -0.5 ; clear <> -1 .. -0.9 {saw 0.1}; r <> saw 40m..45m"
   hideableWidget referenceVisible "referenceText" $ text "generates a triangle wave."
   return ()
