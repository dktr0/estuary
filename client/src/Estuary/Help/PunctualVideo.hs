{-# LANGUAGE OverloadedStrings #-}
module Estuary.Help.PunctualVideo where

import Reflex
import Reflex.Dom
import Data.Text
import GHCJS.DOM.EventM
import Estuary.Widgets.Generic


--render multiple sub-help files
punctualVideoHelpFile :: MonadWidget t m => m ()
punctualVideoHelpFile = divClass "languageHelp" $ do
    about
    functionRef "sin"
    functionRef "tri"
    functionRef "sqr"
    functionRef "saw"
    functionRef "x"
    functionRef "y"
    functionRef "red"
    functionRef "green"
    functionRef "blue"
    functionRef "alpha"
    functionRef "width"
    functionRef "height"
    -- functionRef "db"
    -- functionRef "<s>"
    -- functionRef "<ms>"
    -- functionRef "<c>"
    -- functionRef "@"
    -- functionRef "@()"
    -- functionRef "@<>"
    -- functionRef ":"
    -- functionRef "+-"
    -- functionRef ".."
    return ()

-- about
about :: MonadWidget t m => m ()
about = do
  divClass "about" $ text "PunctualAudio reference"
  divClass "about" $ text "Punctual is a language for live coding audio and visuals. It allows you to build and change networks of signal processors (oscillators, filters, etc) on the fly. Punctual was created by David Ogborn, building on top of the MusicW synthesis library (by David Ogborn, Spencer Park, Jamie Beverley, and others). Conceptually, Punctual extends the work of Julian Rohrhuber and others on SuperCollider's JITlib notations."

exampleText :: Text -> Text

exampleText "sin" = "sin 0.5 => x"
exampleText "tri" = "tri 0.5 => x"
exampleText "sqr" = "sqr 0.5 => y"
exampleText "saw" = "saw 0.5 => y"
exampleText "x" = "saw 20 => x; sin 20 => y"
exampleText "y" = "saw 10 => x; sin 20 => y"
exampleText "red" = "saw 10m => x; Saw 20m => red; saw 31m  => green; 1 => blue"
exampleText "green" = "saw 10m => x; saw 10m => y; Saw 20m => red; saw 31m  => green; 1 => blue"
exampleText "blue" = "saw 10m => x; Saw 20m => red; saw 31m  => green; 1 => blue"
exampleText "alpha" = "Saw 10m => x; Sin 20 => y; -0.98 => clear; Tri 5m => width; 0.99 => height; Saw 20m => red; saw 31m  => green; 1 => blue; 0 => alpha"
exampleText "width" = "0 => clear; Saw 0m => x; Sin 10 => y; Tri 5m => width; 0.99 => height"
exampleText "height" = "0 => clear; Saw 0m => x; Sin 10 => y; Tri 5m => width; 0.99 => height"
-- exampleText "db" = "sin 57m * -10 db => centre"
-- exampleText "<s>" = "<8s> sin 440 => centre"
-- exampleText "<ms>" = "<2500ms> sin 440 => centre"
-- exampleText "<c>" = "<1.5c> sin 440 => centre"
-- exampleText "@" = "@4c sin 440 => centre"
-- exampleText "@()" = "@(2c,0.5c) sin 440 => centre"
-- exampleText "@<>" = "@2c <2c> sin 440 => centre"
-- exampleText ":" = "sin (440 : sin 1)"
-- exampleText "+-" = "saw (24m +- 3% : sin 1) => centre"
-- exampleText ".." = "lpf (saw 24m) (100 .. 1000 : sin 1) 1 => centre"

referenceText :: Text -> Text

referenceText "sin" = "a sin wave"
referenceText "tri" = "a tri wave"
referenceText "sqr" = "a sqr wave"
referenceText "saw" = "a saw wave"
referenceText "x" = "sets the x coordinate of the pixel"
referenceText "y" = "sets the y coordinat of the pixel"
referenceText "red" = "sets the RGBA red value"
referenceText "green" = "sets the RGBA green value"
referenceText "blue" = "sets the RGBA blue value"
referenceText "alpha" = "sets the RGBA alpha value"
referenceText "width" = "sets the width of the pixel"
referenceText "height" = "sets the height of the pixel"

-- help files for samples
functionRef :: MonadWidget t m => Text -> m ()
functionRef x = divClass "helpWrapper" $ do
   switchToReference <- divClass "refExampleButton" $ button x
   exampleVisible <- toggle True switchToReference
   referenceVisible <- toggle False switchToReference
   hideableWidget exampleVisible "exampleText" $ text (exampleText x)
   hideableWidget referenceVisible "referenceText" $ text (referenceText x)
   return ()
