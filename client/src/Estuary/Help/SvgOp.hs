{-# LANGUAGE OverloadedStrings #-}
module Estuary.Help.SvgOp where

import Reflex
import Reflex.Dom
import GHCJS.DOM.EventM
import Estuary.Widgets.Generic

-- import Estuary.Types.Language

--render multiple sub-help files
svgOpHelpFile :: MonadWidget t m => m ()
svgOpHelpFile = divClass "languageHelp" $ do
    about
    line
    rect
    circle
    ellipse
    triangle
    polyline
    polygon
    return ()

-- about
about :: MonadWidget t m => m ()
about = do
  divClass "about" $ text "SvgOp reference"
  divClass "aboutText" $ text "A mini language mapping SVG and HTML properties."


-- help files for samples
line :: MonadWidget t m => m ()
line = divClass "helpWrapper" $ do
   switchToReference <- divClass "refExampleButton" $ button "line:"
   exampleVisible <- toggle True switchToReference
   referenceVisible <- toggle False switchToReference
   hideableWidget exampleVisible "exampleText" $ text "line 0 0 50 50 s[(100, 70, 96, 100) 5 Butt Miter (70, 80)] t[4 0.5 0.7 (100,8)]"
   hideableWidget referenceVisible "referenceText" $ text "line x1 y1 x2 y2 stroke tranformation" --languageHelpWidget MiniTidal
   return ()


   -- help files for samples
rect :: MonadWidget t m => m ()
rect = divClass "helpWrapper" $ do
   switchToReference <- divClass "refExampleButton" $ button "rect:"
   exampleVisible <- toggle True switchToReference
   referenceVisible <- toggle False switchToReference
   hideableWidget exampleVisible "exampleText" $ text "rect 0 0 50 50 f(90, 75, 40, 100) s[(100, 70, 96, 100) 5 Butt Miter (70, 80)] t[4 0.5 0.7 (100,8)]"
   hideableWidget referenceVisible "referenceText" $ text "rect x y width height fill stroke tranformation"
   return ()


   -- help files for samples
circle :: MonadWidget t m => m ()
circle = divClass "helpWrapper" $ do
   switchToReference <- divClass "refExampleButton" $ button "circle:"
   exampleVisible <- toggle True switchToReference
   referenceVisible <- toggle False switchToReference
   hideableWidget exampleVisible "exampleText" $ text "circle 10 10 10 f(10, 60, 90) s[(100, 70, 96, 100) 5 Butt Miter (70, 80)] t[4 0.5 0.7 (100,8)]"
   hideableWidget referenceVisible "referenceText" $ text "circle x y radius fill stroke tranformation"
   return ()

   -- help files for samples
ellipse :: MonadWidget t m => m ()
ellipse = divClass "helpWrapper" $ do
   switchToReference <- divClass "refExampleButton" $ button "ellipse:"
   exampleVisible <- toggle True switchToReference
   referenceVisible <- toggle False switchToReference
   hideableWidget exampleVisible "exampleText" $ text "ellipse 10 10 20 10 f(10, 200, 9, 50) s[(100, 70, 96, 100) 5 Butt Miter (70, 80)] t[4 0.5 0.7 (100,8)]"
   hideableWidget referenceVisible "referenceText" $ text "ellipse x y width height fill stroke tranformation"
   return ()


-- help files for functions
triangle :: MonadWidget t m => m ()
triangle = divClass "helpWrapper" $ do
   switchToReference <- divClass "refExampleButton" $ button "triangle:"
   exampleVisible <- toggle True switchToReference
   referenceVisible <- toggle False switchToReference
   hideableWidget exampleVisible "exampleText" $ text "triangle 0 0 0 100 100 100 f(255,0,0) s[(100, 70, 96, 100) 5 Butt Miter (70, 80)] t[4 0.5 0.7 (100,8)]"
   hideableWidget referenceVisible "referenceText" $ text "triangle x1 y1 x2 y2 x3 y3 fill stroke tranformation"
   return ()

-- help files for functions
polyline :: MonadWidget t m => m ()
polyline = divClass "helpWrapper" $ do
   switchToReference <- divClass "refExampleButton" $ button "polyline:"
   exampleVisible <- toggle True switchToReference
   referenceVisible <- toggle False switchToReference
   hideableWidget exampleVisible "exampleText" $ text "polyline 10 10 80 80 90 90 60 90 45 7 f(0, 27, 90) s[(100, 70, 96, 100) 5 Butt Miter (70, 80)] t[4 0.5 0.7 (100,8)]"
   hideableWidget referenceVisible "referenceText" $ text "polyline x1 y1 ... xn yn fill stroke tranformation"
   return ()


-- help files for functions
polygon :: MonadWidget t m => m ()
polygon = divClass "helpWrapper" $ do
   switchToReference <- divClass "refExampleButton" $ button "polygon"
   exampleVisible <- toggle True switchToReference
   referenceVisible <- toggle False switchToReference
   hideableWidget exampleVisible "exampleText" $ text "polygon 65 30 40 20 90 0 80 90 f(0, 27, 90) s[(100, 70, 96, 100) 5 Butt Miter (70, 80)] t[4 0.5 0.7 (100,8)]"
   hideableWidget referenceVisible "referenceText" $ text "polygon x1 y1 ... xn yn fill stroke tranformation"
   return ()
