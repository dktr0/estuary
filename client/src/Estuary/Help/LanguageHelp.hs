{-# LANGUAGE OverloadedStrings #-}

module Estuary.Help.LanguageHelp where

import Reflex
import Reflex.Dom
import Control.Monad.Fix (MonadFix)
import Data.Text (Text)
import qualified Data.Text as Td
import Data.Map
import Control.Monad
import GHCJS.DOM.EventM -- just used for our test, maybe delete-able later

import Estuary.Help.NoHelpFile
import Estuary.Help.MiniTidal
import Estuary.Help.PunctualAudio
import Estuary.Help.Hydra
import Estuary.Help.CineCer0.CineCer0
import Estuary.Help.CineCer0.CineCer0Reference
import Estuary.Types.TextNotation

parserToHelp :: (MonadFix m, PostBuild t m, DomBuilder t m, MonadHold t m) => TextNotation -> m ()
parserToHelp "MiniTidal" = miniTidalHelpFile
parserToHelp "Punctual" = punctualAudioHelpFile
parserToHelp _ = noHelpFile
