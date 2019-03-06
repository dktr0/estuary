{-# LANGUAGE OverloadedStrings #-}
module Estuary.Help.PunctualAudio where

import Reflex
import Reflex.Dom
import Data.Text
import GHCJS.DOM.EventM
import Estuary.Widgets.Generic


--render multiple sub-help files
punctualAudioHelpFile :: MonadWidget t m => m ()
punctualAudioHelpFile = divClass "languageHelp" $ do
    about
    functionRef "sin"
    functionRef "tri"
    functionRef "sqr"
    functionRef "saw"
    functionRef "lpf"
    functionRef "hpf"
    functionRef "=> centre"
    functionRef "=> 50%"
    functionRef "=> 0.5"
    functionRef "=> left"
    functionRef "=> right"
    functionRef "=> 25%"
    functionRef "db"
    functionRef "<s>"
    functionRef "<ms>"
    functionRef "<c>"
    functionRef "@"
    functionRef "@()"
    functionRef "@<>"
    functionRef ":"
    functionRef "+-"
    functionRef ".."
    return ()

-- about
about :: MonadWidget t m => m ()
about = do
  divClass "about" $ text "PunctualAudio reference"
  divClass "about" $ text "Punctual is a language for live coding audio and visuals. It allows you to build and change networks of signal processors (oscillators, filters, etc) on the fly. Punctual was created by David Ogborn, building on top of the MusicW synthesis library (by David Ogborn, Spencer Park, Jamie Beverley, and others). Conceptually, Punctual extends the work of Julian Rohrhuber and others on SuperCollider's JITlib notations."

exampleText :: Text -> Text

exampleText "sin" = "sin 440 => centre"
exampleText "tri" = "tri 440 => centre"
exampleText "sqr" = "sqr 440 => centre"
exampleText "saw" =  "saw 440 => centre"
exampleText "lpf" = "lpf (saw 110) 1000 1 => centre"
exampleText "hpf" = "hpf (saw 110) 1000 1 => centre"
exampleText "=> centre" = "sin 440 => centre"
exampleText "=> 50%" = "sin 440 => 50%"
exampleText "=> 0.5" = "sin 440 => 0.5"
exampleText "=> left" = "sin 440 => left"
exampleText "=> right" = "sin 440 => right"
exampleText "=> 25%" = "sin 440 => 25%"
exampleText "db" = "sin 57m * -10 db => centre"
exampleText "<s>" = "<8s> sin 440 => centre"
exampleText "<ms>" = "<2500ms> sin 440 => centre"
exampleText "<c>" = "<1.5c> sin 440 => centre"
exampleText "@" = "@4c sin 440 => centre"
exampleText "@()" = "@(2c,0.5c) sin 440 => centre"
exampleText "@<>" = "@2c <2c> sin 440 => centre"
exampleText ":" = "sin (440 : sin 1)"
exampleText "+-" = "saw (24m +- 3% : sin 1) => centre"
exampleText ".." = "lpf (saw 24m) (100 .. 1000 : sin 1) 1 => centre"

referenceText :: Text -> Text

referenceText "sin" = "a 440 Hz sine wave"
referenceText "tri" = "a 440 Hz tri wave"
referenceText "sqr" = "a 440 Hz sqr wave"
referenceText "saw" = "a 440 Hz saw wave"
referenceText "lpf" = "a 1000 Hz (Q=1) low-pass filter applied to a sawtooth wave"
referenceText "hpf" = "a 1000 Hz (Q=1) high-pass filter applied to a sawtooth wave"
referenceText "=> centre" = "sound panned to the centre"
referenceText "=> 50%" = "also sound panned to the centre"
referenceText "=> 0.5" = "also sound panned to the centre"
referenceText "=> left" = "sound panned to the left"
referenceText "=> right" = "sound panned to the right"
referenceText "=> 25%" = "sound panned halfway to the left"
referenceText "db" = "a quieter sine wave. You can try more quieter decibels like -13bd or -40db"
referenceText "<s>" = "new definition crossfades over seconds"
referenceText "<ms>" = "crossfade over milliseconds"
referenceText "<c>" = "crossfade over cycles (bars)"
referenceText "@" = "sets a new definition to start on next cycle/bar boundary"
referenceText "@()" = "sets a new definition to start to start half cycle after next two cycle boundary"
referenceText "@<>" = "sets a new definiton starts at next 2-cycle boundary, crossfades over 2 cycles"
referenceText ":" = "makes a wave frequency go between two given numbers"
referenceText "+-" = "makes a wave frequency go between a given percentage below and above a Freeuqncy or a MIDI note"
referenceText ".." = "Filters a frequency between two given numbers"

-- help files for samples
functionRef :: MonadWidget t m => Text -> m ()
functionRef x = divClass "helpWrapper" $ do
   switchToReference <- divClass "refExampleButton" $ button x
   exampleVisible <- toggle True switchToReference
   referenceVisible <- toggle False switchToReference
   hideableWidget exampleVisible "exampleText" $ text (exampleText x)
   hideableWidget referenceVisible "referenceText" $ text (referenceText x)
   return ()
