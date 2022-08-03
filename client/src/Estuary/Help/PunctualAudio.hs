{-# LANGUAGE OverloadedStrings #-}
module Estuary.Help.PunctualAudio where

import Reflex
import Reflex.Dom
import Data.Text
import GHCJS.DOM.EventM
import Estuary.Widgets.Reflex
import Estuary.Widgets.Reflex
import Control.Monad.Fix


--render multiple sub-help files
punctualAudioHelpFile :: (Monad m, MonadFix m, PostBuild t00 m, MonadHold t00 m, DomBuilder t00 m) => m ()
punctualAudioHelpFile = divClass "languageHelpContainer" $ divClass "languageHelp" $ do
    about
    functionRef "sin"
    functionRef "tri"
    functionRef "sqr"
    functionRef "saw"
    functionRef "lpf"
    functionRef "hpf"
    functionRef ">> centre"
    functionRef ">> 0.5"
    functionRef ">> left"
    functionRef ">> right"
    functionRef "db"
    return ()

-- about
about :: DomBuilder t0 m => m ()
about = do
  divClass "about primary-color code-font" $ text "Punctual is a language for live coding audio and visuals. It allows you to build and change networks of signal processors (oscillators, filters, etc) on the fly. Punctual was created by David Ogborn, building on top of the MusicW synthesis library (by David Ogborn, Spencer Park, Jamie Beverley, and others). Conceptually, Punctual extends the work of Julian Rohrhuber and others on SuperCollider's JITlib notations. Complete and up-to-date documentation of Punctual can be found here: https://github.com/dktr0/Punctual"

exampleText :: Text -> Text

exampleText "sin" = "sin 440 >> centre"
exampleText "tri" = "tri 440 >> centre"
exampleText "sqr" = "sqr 440 >> centre"
exampleText "saw" =  "saw 440 >> centre"
exampleText "lpf" = "lpf 1000 1 (saw 110) >> centre"
exampleText "hpf" = "hpf 1000 1 (saw 110) >> centre"
exampleText ">> centre" = "sin 440 >> centre"
exampleText ">> 0.5" = "sin 440 >> 0.5"
exampleText ">> left" = "sin 440 >> left"
exampleText ">> right" = "sin 440 >> right"
exampleText "db" = "sin 440 * (-10) db >> centre"

referenceText :: Text -> Text

referenceText "sin" = "a 440 Hz sine wave"
referenceText "tri" = "a 440 Hz tri wave"
referenceText "sqr" = "a 440 Hz sqr wave"
referenceText "saw" = "a 440 Hz saw wave"
referenceText "lpf" = "a 1000 Hz (Q=1) low-pass filter applied to a sawtooth wave"
referenceText "hpf" = "a 1000 Hz (Q=1) high-pass filter applied to a sawtooth wave"
referenceText ">> centre" = "sound panned to the centre"
referenceText ">> 0.5" = "also sound panned to the centre"
referenceText ">> left" = "sound panned to the left"
referenceText ">> right" = "sound panned to the right"
referenceText "db" = "a quieter sine wave. You can try quieter decibels like -13db or -40db"

-- help files for samples
functionRef :: (Monad m, Reflex t0, MonadHold t0 m, PostBuild t0 m, DomBuilder t0 m, MonadFix m) => Text -> m ()
functionRef x = divClass "helpWrapper" $ do
   switchToReference <- buttonWithClass' x
   exampleVisible <- toggle True switchToReference
   referenceVisible <- toggle False switchToReference
   hideableWidget exampleVisible "exampleText primary-color code-font" $ text (exampleText x)
   hideableWidget referenceVisible "referenceText code-font" $ text (referenceText x)
   return ()
